import type { PrerenderFn } from "@wasp.sh/lib-vite-ssr/types";
import * as streamConsumers from "node:stream/consumers";
import assert from "node:assert/strict";
import { prerenderToNodeStream as reactPrerender } from "react-dom/static";
import {
  createStaticHandler,
  createStaticRouter,
  StaticRouterProvider,
  type HydrationState,
} from "react-router";

import { Layout } from "wasp/client/app/layout";
import { WaspApp } from "wasp/client/app";

import { routeObjects } from '/@wasp/routes.tsx'

const WASP_SSR_DATA_KEY = "__WASP_SSR_DATA__" satisfies keyof typeof globalThis;
const SPA_FALLBACK_FILE = "200.html";

const prerenderApp: PrerenderFn = async (route, { clientEntrySrc }) => {
  const { query, dataRoutes } = createStaticHandler(routeObjects, {
    basename: "/",
  });

  const req = new Request(new URL(route, "http://localhost"));

  const context = await query(req);
  assert(
    !(context instanceof Response),
    "Redirects from React Router's `loader`s are not supported",
  );

  const router = createStaticRouter(dataRoutes, context);
  const isFallbackPage = route === SPA_FALLBACK_FILE;

  function App() {
    return (
      <Layout isFallbackPage={isFallbackPage} clientEntrySrc={clientEntrySrc}>
        <WaspApp>
          {/*
            `hydrate={false}` because the default hydration script would be
            rendered inside the React tree, causing a hydration mismatch. We
            serialize the hydration data ourselves in `bootstrapScriptContent`,
            which is React's recommended way of adding SSR-only script tags.
          */}
          <StaticRouterProvider router={router} context={context} hydrate={false} />
        </WaspApp>
      </Layout>
    )
  }

  // The fallback page's static handler context is a 404 (nothing matches the
  // literal fallback file URL), so its state must not leak into the client router.
  const routerHydrationData: HydrationState | undefined =
    isFallbackPage
      ? undefined
      : context;

  const waspSsrData: WaspSSRData = { isFallbackPage, routerHydrationData }

  const html = await reactPrerender(<App/>, {
    bootstrapScriptContent: `window.${WASP_SSR_DATA_KEY}=${JSON.stringify(waspSsrData)};`,
  })
    .then((result) => streamConsumers.text(result.prelude))

  return html;
}

export default prerenderApp;
