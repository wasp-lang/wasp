import { startTransition } from "react";
import { hydrateRoot } from "react-dom/client";
import { createBrowserRouter } from "react-router";
import { RouterProvider } from "react-router/dom";

import { Layout } from "wasp/client/app/layout";
import {
  listenForUncaughtClientErrors,
  registerClientOnErrorFn,
  reportRenderError,
  WaspApp,
} from "wasp/client/app";

import type { ClientOnErrorFn } from "wasp/client";
import { onClientError as onClientError_ext } from "./src/features/errors/clientOnError"

import { routeObjects } from "/@wasp/routes.tsx"

const router = createBrowserRouter(routeObjects, {
  basename: "/",
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

registerClientOnErrorFn(onClientError_ext satisfies ClientOnErrorFn);
listenForUncaughtClientErrors();

startTransition(() => {
  hydrateRoot(document, fullAppTree, {
    // React calls these for errors thrown while rendering, with the stack of
    // components that led to the error.
    // https://react.dev/reference/react-dom/client/hydrateRoot#parameters
    onUncaughtError: reportRenderError,
    onCaughtError: reportRenderError,
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
