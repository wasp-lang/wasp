{{={= =}=}}
// @ts-nocheck
import * as React from "react";
import { Outlet, createMemoryRouter, matchRoutes, RouterProvider } from "react-router";
import { renderToString } from "react-dom/server";
import { QueryClientProvider } from "@tanstack/react-query";

// SSR Styles Provider – generic hook for CSS-in-JS libraries.
//
// If the user creates `src/ssr/styles.tsx` (or .ts/.js) and exports a
// `createSsrStylesProvider` function, Wasp uses it to:
//   1. Wrap the React tree (e.g. Emotion CacheProvider, SC StyleSheetManager)
//   2. Extract critical CSS after renderToString and inject it into <head>
//
// This is package-agnostic: works with Emotion, styled-components, Stitches,
// or any other CSS-in-JS library.  See `wasp/client/ssr` for the type API.
//
// If the file doesn't exist, SSR proceeds without CSS-in-JS integration.
import type { CreateSsrStylesProvider } from "wasp/client/ssr";

let createSsrStylesProvider: CreateSsrStylesProvider | null = null;
try {
  const mod = await import("./src/ssr/styles");
  createSsrStylesProvider = mod.createSsrStylesProvider || mod.default || null;
} catch {
  // No user-provided SSR styles configuration – skip.
}

import { initializeQueryClient, queryClientInitialized } from "wasp/client/operations";
import { routes } from "wasp/client/router";
import { DefaultRootErrorBoundary } from "wasp/client/app/components/DefaultRootErrorBoundary";

{=# isExternalAuthEnabled =}
import { OAuthCallbackPage } from "wasp/client/app/pages/OAuthCallback";
{=/ isExternalAuthEnabled =}

{=& routesMapping.importStatement =}
{=& routeNameToSsr.importStatement =}
{=& routeNameToHead.importStatement =}

{=# rootComponent.isDefined =}
{=& rootComponent.importStatement =}
{=/ rootComponent.isDefined =}

// NOTE: We intentionally do NOT call the client setupFn during SSR.
// The setup function is for client-side initialization (React Query config,
// analytics, etc.) which doesn't apply to server-side rendering.
// Any operations called in setupFn would fail in SSR context anyway.

const baseDir = "{= baseDir =}";

const DefaultRootComponent = () => <Outlet />;
const rootElement =
  {=# rootComponent.isDefined =}
  <{= rootComponent.importIdentifier =} />
  {=/ rootComponent.isDefined =}
  {=^ rootComponent.isDefined =}
  <DefaultRootComponent />
  {=/ rootComponent.isDefined =}

initializeQueryClient();

function createRouteObjects() {
  const waspDefinedRoutes = [
    {=# isExternalAuthEnabled =}
    {
      path: "{= oAuthCallbackPath =}",
      Component: OAuthCallbackPage,
      handle: { ssr: false },
    },
    {=/ isExternalAuthEnabled =}
  ];

  const userDefinedRoutes = Object.entries(routes).map(([routeKey, route]) => ({
    path: route.to,
    Component: routesMapping[routeKey],
    handle: {
      ssr: routeNameToSsr[routeKey] ?? false,
      head: routeNameToHead[routeKey],
    },
  }));

  return [
    {
      path: "/",
      element: rootElement,
      ErrorBoundary: DefaultRootErrorBoundary,
      children: [...waspDefinedRoutes, ...userDefinedRoutes],
    },
  ];
}

function stripBase(pathname: string): string | null {
  if (baseDir === "/" || baseDir === "") {
    return pathname;
  }
  const normalizedBase = baseDir.endsWith("/")
    ? baseDir.slice(0, -1)
    : baseDir;
  if (pathname === normalizedBase) {
    return "/";
  }
  if (pathname.startsWith(normalizedBase + "/")) {
    return pathname.slice(normalizedBase.length);
  }
  return null;
}

export function getRouteMatchInfo(url: string): {
  matched: boolean;
  ssr: boolean;
  outsideBase: boolean;
  isCatchAll: boolean;
} {
  const pathname = new URL(url, "http://localhost").pathname;
  const strippedPathname = stripBase(pathname);
  if (!strippedPathname) {
    return { matched: false, ssr: false, outsideBase: true, isCatchAll: false };
  }

  const matches = matchRoutes(createRouteObjects(), strippedPathname);
  // Check if the deepest matched route is a catch-all (wildcard) route
  const deepestMatch = matches?.[matches.length - 1];
  const isCatchAll = deepestMatch?.route?.path === "*";

  return {
    matched: matches !== null,
    ssr: matches?.some((match) => match.route.handle?.ssr) ?? false,
    outsideBase: false,
    isCatchAll,
  };
}

export async function render(url: string): Promise<{
  appHtml: string;
  headHtml: string;
  hasPageTitle: boolean;
}> {
  const queryClient = await queryClientInitialized;
  queryClient.clear();

  const { headHtml, hasTitle } = await resolveHeadHtml(url);

  const router = createMemoryRouter(createRouteObjects(), {
    initialEntries: [url],
    basename: baseDir,
  });

  let appTree = (
    <QueryClientProvider client={queryClient}>
      <RouterProvider router={router} />
    </QueryClientProvider>
  );

  // If the user provided an SSR styles provider, initialize it for this
  // request and wrap the app tree with its Wrapper component.
  const ssrStyles = createSsrStylesProvider?.() ?? null;

  if (ssrStyles?.Wrapper) {
    const SsrWrapper = ssrStyles.Wrapper;
    appTree = <SsrWrapper>{appTree}</SsrWrapper>;
  }

  const appHtml = renderToString(appTree);

  // After rendering, let the provider extract critical CSS for the <head>.
  let stylesHtml = "";
  if (ssrStyles?.extractStyles) {
    try {
      stylesHtml = ssrStyles.extractStyles(appHtml);
    } catch (e) {
      console.warn("[wasp:ssr] extractStyles failed:", e);
    }
  }

  return {
    appHtml,
    headHtml: stylesHtml + headHtml,
    hasPageTitle: hasTitle,
  };
}

async function resolveHeadHtml(url: string): Promise<{
  headHtml: string;
  hasTitle: boolean;
}> {
  const pathname = new URL(url, "http://localhost").pathname;
  const strippedPathname = stripBase(pathname);
  if (!strippedPathname) {
    return { headHtml: "", hasTitle: false };
  }

  const matches = matchRoutes(createRouteObjects(), strippedPathname);
  if (!matches) {
    return { headHtml: "", hasTitle: false };
  }

  for (let i = matches.length - 1; i >= 0; i--) {
    const headExport = matches[i].route.handle?.head;
    if (!headExport) {
      continue;
    }
    const headValue =
      typeof headExport === "function" ? await headExport() : headExport;
    return renderHeadToString(headValue);
  }

  return { headHtml: "", hasTitle: false };
}

function renderHeadToString(headValue: unknown): {
  headHtml: string;
  hasTitle: boolean;
} {
  if (!headValue) {
    return { headHtml: "", hasTitle: false };
  }

  if (typeof headValue !== "object") {
    return { headHtml: "", hasTitle: false };
  }

  const head = headValue as {
    title?: string;
    meta?: Record<string, string>[];
    link?: Record<string, string>[];
  };

  const parts: string[] = [];
  const hasTitle = Boolean(head.title);
  if (hasTitle) {
    parts.push(`<title>${escapeHtml(head.title!)}</title>`);
  }
  if (Array.isArray(head.meta)) {
    for (const meta of head.meta) {
      parts.push(`<meta ${renderAttributes(meta)} />`);
    }
  }
  if (Array.isArray(head.link)) {
    for (const link of head.link) {
      parts.push(`<link ${renderAttributes(link)} />`);
    }
  }

  return { headHtml: parts.filter(Boolean).join(""), hasTitle };
}

function renderAttributes(attrs: Record<string, string>): string {
  return Object.entries(attrs)
    .filter(([, value]) => value !== undefined && value !== null)
    .map(([key, value]) => `${key}="${escapeHtml(String(value))}"`)
    .join(" ");
}

function escapeHtml(input: string): string {
  return input
    .replace(/&/g, "&amp;")
    .replace(/</g, "&lt;")
    .replace(/>/g, "&gt;")
    .replace(/"/g, "&quot;")
    .replace(/'/g, "&#39;");
}
