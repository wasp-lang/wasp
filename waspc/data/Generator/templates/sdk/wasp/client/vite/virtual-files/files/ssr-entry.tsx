{{={= =}=}}
// @ts-nocheck
import type { PrerenderContext, PrerenderFn } from "@wasp.sh/lib-vite-ssr/types";
import { text } from "node:stream/consumers";
import type { ReactNode } from "react";
import { prerenderToNodeStream as reactPrerender } from "react-dom/static";
import {
  createStaticHandler,
  createStaticRouter,
  RouterProvider,
} from "react-router";
import { Layout } from "wasp/client/app/layout";
import { WaspApp } from "wasp/client/app";
import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";

{=& routesMapping.importStatement =}

{=# rootComponent.isDefined =}
{=& rootComponent.importStatement =}
{=/ rootComponent.isDefined =}

{=# setupFn.isDefined =}
await {= setupFn.importIdentifier =}()
{=/ setupFn.isDefined =}

initializeQueryClient()

const rootElement =
  {=# rootComponent.isDefined =}
  <{= rootComponent.importIdentifier =} />
  {=/ rootComponent.isDefined =}
  {=^ rootComponent.isDefined =}
  undefined
  {=/ rootComponent.isDefined =}

const routeObjects = getRouteObjects({
  routesMapping: {= routesMapping.importIdentifier =},
  rootElement,
})

const prerenderApp = async (
  {
    isFallbackPage,
    children,
  }: { isFallbackPage: boolean; children?: ReactNode },
  { clientEntrySrc, transformIndexHtml }: PrerenderContext,
) => {
  const app = (
    <Layout isFallbackPage={isFallbackPage} clientEntrySrc={clientEntrySrc}>
      <WaspApp>
        {children}
      </WaspApp>
    </Layout>
  );

  const html = await reactPrerender(app)
    .then((result) => text(result.prelude))
    .then((html) => transformIndexHtml(html));

  return new Response(html, {
    status: 200,
    headers: { "Content-Type": "text/html; charset=utf-8" },
  });
};

const { query, dataRoutes } = createStaticHandler(routeObjects, {
  basename: "{= baseDir =}",
});

const prerenderWithRouter: PrerenderFn = async (route, ctx) => {
  const req = new Request(new URL(route, "http://localhost"));

  const context = await query(req);

  if (context instanceof Response) {
    return context;
  }

  const router = createStaticRouter(dataRoutes, context);

  const res = await prerenderApp(
    { isFallbackPage: false, children: <RouterProvider router={router} /> },
    ctx,
  );

  const leaf = context.matches[context.matches.length - 1];
  const actionHeaders = context.actionHeaders[leaf.route.id];
  const loaderHeaders = context.loaderHeaders[leaf.route.id];

  assignHeaders(res.headers, actionHeaders);
  assignHeaders(res.headers, loaderHeaders);

  return new Response(res.body, {
    status: context.statusCode,
    headers: res.headers,
    statusText: res.statusText,
  });
};

const assignHeaders = (target: Headers, source?: Headers) => {
  if (!source) return;
  for (const [key, value] of source.entries()) {
    target.append(key, value);
  }
};

const prerenderWithoutRouter = async (ctx: PrerenderContext) => {
  return await prerenderApp({ isFallbackPage: true, children: null }, ctx);
};

const FALLBACK_PATH = "{= ssrFallbackPath =}";

const prerender: PrerenderFn = async (route, ctx) => {
  const isFallbackPage = route === FALLBACK_PATH;
  if (isFallbackPage) {
    return prerenderWithoutRouter(ctx);
  } else {
    return prerenderWithRouter(route, ctx);
  }
};

export default prerender;
