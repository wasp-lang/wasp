{{={= =}=}}
import { startTransition } from "react";
import { hydrateRoot } from "react-dom/client";
import { createBrowserRouter } from "react-router";
import { RouterProvider } from "react-router/dom";

import { Layout } from "wasp/client/app/layout";
import { WaspApp } from "wasp/client/app";
import {
  handleClientError,
  listenForUncaughtClientErrors,
} from "@wasp.sh/lib-sdk-core/browser";

{=& routeObjects.importStatement =}

const router = createBrowserRouter({= routeObjects.importIdentifier =}, {
  basename: {=& baseDir =},
  // React Router will put hydration data on this property of the `window` object.
  // https://reactrouter.com/8.0.1/start/data/custom#4-hydrate-in-the-browser
  hydrationData: window.__staticRouterHydrationData,
})

// We embed this data at prerendering time.
const { isFallbackPage } = window.__WASP_SSR_DATA__ ?? {}

const routerProviderPromise =
  waitForRouterInitialized(router).then(() => (
    <RouterProvider router={router} />
  ))

const fullAppTree = (
  <Layout isFallbackPage={isFallbackPage}>
    <WaspApp>
      {routerProviderPromise}
    </WaspApp>
  </Layout>
)

listenForUncaughtClientErrors();

startTransition(() => {
  hydrateRoot(document, fullAppTree, {
    // The root error boundary reports render errors inside the router. React
    // calls this for render errors no error boundary caught, e.g. in the
    // layout around the router.
    // https://react.dev/reference/react-dom/client/hydrateRoot#parameters
    onUncaughtError: (error: unknown) => {
      handleClientError(error, { source: "render" });
    },
  });
});

async function waitForRouterInitialized(
  router: ReturnType<typeof createBrowserRouter>,
): Promise<void> {
  if (router.state.initialized) {
    return;
  }

  return new Promise((resolve) => {
    const unsubscribe = router.subscribe(() => {
      if (router.state.initialized) {
        unsubscribe();
        resolve();
      }
    });
  });
}
