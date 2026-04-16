{{={= =}=}}
import type { PrerenderFn } from "@wasp.sh/lib-vite-ssr/types";
import * as streamConsumers from "node:stream/consumers";
import assert from "node:assert/strict";
import { prerenderToNodeStream as reactPrerender } from "react-dom/static";
import {
  createStaticHandler,
  createStaticRouter,
  RouterProvider,
} from "react-router";

import { Layout } from "wasp/client/app/layout";
import { WaspApp } from "wasp/client/app";
import { queryClientPromise } from "wasp/client/operations/bootstrap";

{=& routeObjects.importStatement =}

const SPA_FALLBACK_FILE = "{= spaFallbackFile =}";

const prerenderApp: PrerenderFn = async (route, { clientEntrySrc }) => {
  const { query, dataRoutes } = createStaticHandler({= routeObjects.importIdentifier =}, {
    basename: "{= baseDir =}",
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
          <RouterProvider router={router} />
        </WaspApp>
      </Layout>
    )
  }

  const WASP_SSR_DATA: WaspSSRData = { isFallbackPage }

  await queryClientPromise;

  const html = await reactPrerender(<App/>, {
    bootstrapScriptContent: `window.__WASP_SSR_DATA__=${JSON.stringify(WASP_SSR_DATA)};`,
  })
    .then((result) => streamConsumers.text(result.prelude))

  return html;
}

export default prerenderApp;
