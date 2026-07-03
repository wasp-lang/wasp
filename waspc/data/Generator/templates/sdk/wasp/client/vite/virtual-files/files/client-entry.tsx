{{={= =}=}}
import { startTransition } from "react";
import { hydrateRoot } from "react-dom/client";
import { createBrowserRouter, type DataRouter } from "react-router";
import { RouterProvider } from "react-router/dom";

import { Layout } from "wasp/client/app/layout";
import { WaspApp } from "wasp/client/app";

{=& routeObjects.importStatement =}

const router = createBrowserRouter({= routeObjects.importIdentifier =}, {
  basename: "{= baseDir =}",
  // React Router will put hydration data on this property of the `window` object.
  // https://reactrouter.com/7.13.1/start/data/custom#4-hydrate-in-the-browser
  hydrationData: window.__staticRouterHydrationData,
})

// Rendering `RouterProvider` before the router has loaded the matched route's
// `lazy` module would commit its `HydrateFallback` over the prerendered HTML.
// https://github.com/remix-run/react-router/issues/14955#issuecomment-4407850287
function waitForRouterInitialized(router: DataRouter): Promise<DataRouter> {
  return new Promise((resolve) => {
    // The router initializes synchronously when there is nothing to load lazily.
    if (router.state.initialized) {
      resolve(router)
      return
    }

    const unsubscribe = router.subscribe((state) => {
      if (state.initialized) {
        unsubscribe()
        resolve(router)
      }
    })
  })
}

// We embed this data at prerendering time.
const { isFallbackPage } = window.__WASP_SSR_DATA__ ?? {}

// Created once, outside of render: re-renders (e.g. hydration mismatch
// recovery) must see the same, already-settled promise so React can unwrap it
// synchronously instead of suspending again.
const routerProvider = waitForRouterInitialized(router).then((router) => (
  <RouterProvider router={router} />
))

function App() {
  return (
    <Layout isFallbackPage={isFallbackPage}>
      <WaspApp>{routerProvider}</WaspApp>
    </Layout>
  );
}

startTransition(() => {
  hydrateRoot(document, <App />);
});
