import type { PrerenderFn } from "@wasp.sh/lib-vite-ssr/types";
import * as streamConsumers from "node:stream/consumers";
import assert from "node:assert/strict";
import { prerenderToNodeStream as reactPrerender } from "react-dom/static";
import {
  createStaticHandler,
  createStaticRouter,
  StaticRouterProvider,
} from "react-router";

import { Layout } from "wasp/client/app/layout";
import { WaspApp } from "wasp/client/app";

import { routeObjects } from '/@wasp/routes.tsx'

const SPA_FALLBACK_FILE = "200.html";

const prerenderApp: PrerenderFn = async (route, { clientEntrySrc }) => {
  const isFallbackPage = route === SPA_FALLBACK_FILE;

  const staticRouterProvider = isFallbackPage ? undefined : (await makeStaticRouterProvider(route));

  const fullAppTree = (
    <Layout isFallbackPage={isFallbackPage} clientEntrySrc={clientEntrySrc}>
      <WaspApp>
        {staticRouterProvider}
      </WaspApp>
    </Layout>
  )

  const WASP_SSR_DATA: WaspSSRData = { isFallbackPage }

  const html = await reactPrerender(fullAppTree, {
    bootstrapScriptContent: `window.__WASP_SSR_DATA__=${JSON.stringify(WASP_SSR_DATA)};`,
  })
    .then((result) => streamConsumers.text(result.prelude))

  return html;
}

export default prerenderApp;

const { query, dataRoutes } = createStaticHandler(routeObjects, {
  basename: "/",
});

async function makeStaticRouterProvider(route: string) {
  const req = new Request(new URL(route, "http://localhost"));

  const context = await query(req);
  assert(
    !(context instanceof Response),
    "Redirects from React Router's `loader`s are not supported",
  );

  const router = createStaticRouter(dataRoutes, context);

  return <StaticRouterProvider router={router} context={context} />;
}
