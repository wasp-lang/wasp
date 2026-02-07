{{={= =}=}}
// @ts-nocheck
import * as React from "react";
import { Outlet, createMemoryRouter, matchRoutes, RouterProvider } from "react-router";
import { renderToString } from "react-dom/server";
import { QueryClientProvider } from "@tanstack/react-query";

// Emotion SSR support – three things are needed:
//  1. CacheProvider with a fresh cache per render so styled() components
//     don't crash ("Cannot read properties of null (reading 'registered')").
//  2. extractCriticalToChunks + constructStyleTagsFromChunks from
//     @emotion/server to collect the generated CSS and inject it into the
//     HTML <head>.  This eliminates the flash of unstyled content (FOUC)
//     and gives the client Emotion instance the styles it needs for
//     seamless hydration (no mismatch warnings).
//  3. Graceful fallback when @emotion packages are not installed.
let CacheProvider: React.ComponentType<{ value: any; children: React.ReactNode }> | null = null;
let createEmotionCache: ((options: { key: string }) => any) | null = null;
let createEmotionServer: ((cache: any) => {
  extractCriticalToChunks: (html: string) => any;
  constructStyleTagsFromChunks: (chunks: any) => string;
}) | null = null;
try {
  const emotionReact = await import("@emotion/react");
  const emotionCache = await import("@emotion/cache");
  CacheProvider = emotionReact.CacheProvider;
  createEmotionCache = emotionCache.default || emotionCache;
  try {
    const emotionServer = await import("@emotion/server/create-instance");
    createEmotionServer = emotionServer.default || emotionServer;
  } catch {
    // @emotion/server not installed – CacheProvider still prevents the
    // crash but CSS won't be extracted (client will re-generate styles).
  }
} catch {
  // @emotion packages not installed – skip CacheProvider wrapping.
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

  // Wrap with Emotion CacheProvider if available, so styled components
  // have a valid cache during server-side rendering.
  //
  // Two approaches work depending on whether @emotion/server is installed:
  //
  //  Advanced (preferred): createEmotionServer(cache) is called BEFORE
  //    renderToString, which sets cache.compat = true.  This makes the
  //    internal _insert function store the actual CSS text (not just `true`)
  //    in cache.inserted, and suppresses the inline <style> tags that the
  //    Default Approach would emit.  After render, extractCriticalToChunks
  //    collects the CSS and we place it in the <head>.  No nth-child issues.
  //
  //  Default (fallback): If @emotion/server is not installed, Emotion's
  //    built-in SSR emits inline <style> tags before each styled element
  //    during renderToString.  This works thanks to the `typeof document`
  //    define replacement in waspConfig.ts that makes isBrowser = false.
  let ssrCache: any = null;
  let emotionExtractor: {
    extractCriticalToChunks: (html: string) => any;
    constructStyleTagsFromChunks: (chunks: any) => string;
  } | null = null;

  if (CacheProvider && createEmotionCache) {
    ssrCache = createEmotionCache({ key: "css" });

    // MUST happen BEFORE renderToString so that cache.compat = true during
    // style insertion.  This switches _insert to store CSS text and prevents
    // the Default Approach from emitting inline <style> tags.
    if (createEmotionServer) {
      emotionExtractor = createEmotionServer(ssrCache);
    }

    appTree = <CacheProvider value={ssrCache}>{appTree}</CacheProvider>;
  }

  const appHtml = renderToString(appTree);

  // Extract critical CSS from the Emotion cache and prepend it to headHtml.
  // This ensures MUI/Emotion styles arrive with the initial HTML, preventing
  // FOUC and giving the client's Emotion instance the data it needs for
  // seamless hydration (matching class names + existing <style> tags).
  let emotionStyleTags = "";
  if (emotionExtractor) {
    const chunks = emotionExtractor.extractCriticalToChunks(appHtml);
    emotionStyleTags = emotionExtractor.constructStyleTagsFromChunks(chunks);
  }

  return {
    appHtml,
    headHtml: emotionStyleTags + headHtml,
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
