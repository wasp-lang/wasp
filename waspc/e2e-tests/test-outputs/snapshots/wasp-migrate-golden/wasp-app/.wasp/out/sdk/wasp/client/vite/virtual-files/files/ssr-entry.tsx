import { Readable } from "node:stream";
import * as streamConsumers from "node:stream/consumers";
import { type ReactNode } from "react";
import { prerenderToNodeStream as reactPrerender } from "react-dom/static";
import {
  createStaticHandler,
  createStaticRouter,
  StaticRouterProvider,
} from "react-router";

import { Layout } from "wasp/client/app/layout";
import { WaspApp } from "wasp/client/app";

// The tags that load the app in the browser. In dev, `?assets=client` only
// knows about the entry script, while the stylesheets come from `?assets=ssr`
// (they are the ones Vite's dev client hot-updates). In a build,
// `?assets=client` already lists every stylesheet reachable from the browser
// app, so merging the ssr ones would only duplicate them.
import clientAssets from "/@wasp/client-entry.tsx?assets=client";
import ssrAssets from "/@wasp/ssr-entry.tsx?assets=ssr";

import { routeObjects } from '/@wasp/routes.tsx'

const baseDir = "/";

// The paths the user asked us to prerender. They get a fully rendered page,
// everything else gets the SPA shell.
const prerenderedPaths = new Set<string>([]);

const assets = import.meta.env.DEV
  ? clientAssets.merge(ssrAssets)
  : clientAssets;

const { query, dataRoutes } = createStaticHandler(routeObjects, {
  basename: baseDir,
});

export default { fetch: renderPage };

async function renderPage(request: Request): Promise<Response> {
  const pathname = removeBaseDir(new URL(request.url).pathname);

  if (!prerenderedPaths.has(pathname)) {
    return htmlResponse(await getSpaShell());
  }

  const staticRouterProviderOrRedirect = await makeStaticRouterProvider(request);
  // React Router's `loader`s can redirect. We pass the redirect through
  // instead of rendering a page for it.
  if (staticRouterProviderOrRedirect instanceof Response) {
    return staticRouterProviderOrRedirect;
  }

  const result = await reactPrerender(
    buildAppTree({
      isFallbackPage: false,
      children: staticRouterProviderOrRedirect,
    }),
    { bootstrapScriptContent: makeWaspSsrDataScript(false) },
  );

  return htmlResponse(Readable.toWeb(result.prelude) as ReadableStream);
}

let spaShellPromise: Promise<string> | undefined;

/**
 * The SPA shell is the same document for every page that isn't prerendered, so
 * we only render it once. In dev, we render it on every request because the
 * app it depends on changes while the server is running.
 */
function getSpaShell(): Promise<string> {
  if (import.meta.env.DEV) {
    return renderSpaShell();
  }
  return (spaShellPromise ??= renderSpaShell());
}

async function renderSpaShell(): Promise<string> {
  const result = await reactPrerender(
    buildAppTree({ isFallbackPage: true }),
    { bootstrapScriptContent: makeWaspSsrDataScript(true) },
  );
  return streamConsumers.text(result.prelude);
}

function buildAppTree({
  isFallbackPage,
  children,
}: {
  isFallbackPage: boolean;
  children?: ReactNode;
}) {
  return (
    <Layout isFallbackPage={isFallbackPage} headChildren={<AssetTags />}>
      <WaspApp>{children}</WaspApp>
    </Layout>
  );
}

function AssetTags() {
  return (
    <>
      {assets.css.map((attributes) => (
        <link key={attributes.href} rel="stylesheet" {...attributes} />
      ))}
      {assets.js
        // The entry gets its own `<script>` tag below, no need to preload it.
        .filter((attributes) => attributes.href !== assets.entry)
        .map((attributes) => (
          <link key={attributes.href} rel="modulepreload" {...attributes} />
        ))}
      {assets.entry ? <script type="module" src={assets.entry} /> : null}
    </>
  );
}

async function makeStaticRouterProvider(request: Request) {
  const context = await query(request);
  if (context instanceof Response) {
    return context;
  }

  const router = createStaticRouter(dataRoutes, context);

  return <StaticRouterProvider router={router} context={context} />;
}

function makeWaspSsrDataScript(isFallbackPage: boolean): string {
  const waspSsrData: WaspSSRData = { isFallbackPage };
  return `window.__WASP_SSR_DATA__=${JSON.stringify(waspSsrData)};`;
}

/**
 * Nitro hands us the path as the browser requested it, base directory
 * included, while the paths we prerender are relative to the base directory.
 */
function removeBaseDir(pathname: string): string {
  if (baseDir === "/") {
    return pathname;
  }

  const baseDirWithoutTrailingSlash = baseDir.replace(/\/$/, "");
  if (pathname === baseDirWithoutTrailingSlash) {
    return "/";
  }
  if (pathname.startsWith(baseDir)) {
    return pathname.slice(baseDirWithoutTrailingSlash.length);
  }
  return pathname;
}

function htmlResponse(body: string | ReadableStream): Response {
  return new Response(body, {
    headers: { "content-type": "text/html; charset=utf-8" },
  });
}
