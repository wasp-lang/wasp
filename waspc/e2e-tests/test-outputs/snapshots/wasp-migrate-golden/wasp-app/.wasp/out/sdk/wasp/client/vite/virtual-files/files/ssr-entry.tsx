// @ts-nocheck
import type { PrerenderContext, PrerenderFn } from "@wasp.sh/lib-vite-ssr/types";
import * as streamConsumers from "node:stream/consumers";
import assert from "node:assert/strict";
import type { ReactNode } from "react";
import { prerenderToNodeStream as reactPrerender } from "react-dom/static";
import {
  createStaticHandler,
  createStaticRouter,
  RouterProvider,
} from "react-router";
import { Layout } from "wasp/client/app/layout";
import { WaspApp } from "wasp/client/app";

import { routeObjects } from "/@wasp/routes.tsx"

const FALLBACK_FILE = "/_fallback.html";

export default ((async (route, ctx) => {
  const isFallbackPage = route === FALLBACK_FILE;

  if (isFallbackPage) {
     return await prerenderApp({ isFallbackPage: true, children: null }, ctx);
  } else {

    const { query, dataRoutes } = createStaticHandler(routeObjects, {
      basename: "/",
    });

    const req = new Request(new URL(route, "http://localhost"));

    const context = await query(req);
    assert (!(context instanceof Response), "Expected no redirect responses from static handler");

    const router = createStaticRouter(dataRoutes, context);

    return await prerenderApp(
      { isFallbackPage: false, children: <RouterProvider router={router} /> },
      ctx,
    );
  }
}) as PrerenderFn);

async function prerenderApp(
  {
    isFallbackPage,
    children,
  }: { isFallbackPage: boolean; children?: ReactNode },
  { clientEntrySrc, transformIndexHtml }: PrerenderContext,
) {
  const app = (
    <Layout isFallbackPage={isFallbackPage} clientEntrySrc={clientEntrySrc}>
      <WaspApp>
        {children}
      </WaspApp>
    </Layout>
  );

  const html = await reactPrerender(app)
    .then((result) => streamConsumers.text(result.prelude))
    .then((html) => transformIndexHtml(html));

  return html;
};
