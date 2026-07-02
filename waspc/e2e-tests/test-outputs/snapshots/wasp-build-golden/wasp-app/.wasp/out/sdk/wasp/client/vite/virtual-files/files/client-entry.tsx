import { startTransition, use } from "react";
import { hydrateRoot } from "react-dom/client";
import { createBrowserRouter } from "react-router";
import { RouterProvider } from "react-router/dom";

import { Layout } from "wasp/client/app/layout";
import { WaspApp } from "wasp/client/app";

import { routeObjects } from '/@wasp/routes.tsx'

const router = createBrowserRouter(routeObjects, {
  basename: "/",
  // React Router will put hydration data on this property of the `window` object.
  // https://reactrouter.com/7.13.1/start/data/custom#4-hydrate-in-the-browser
  hydrationData: window.__staticRouterHydrationData,
})

// The router is not ready to render until it has loaded the matched route's
// `lazy` module. If `RouterProvider` rendered before that, it would commit its
// `HydrateFallback` over the prerendered HTML, causing a hydration mismatch.
// The React Router team recommends waiting until the router reports
// `state.initialized`:
// https://github.com/remix-run/react-router/issues/14955#issuecomment-4407850287
const routerInitialized = new Promise<void>((resolve) => {
  // `createBrowserRouter` initializes the router synchronously when there is
  // nothing to load lazily, and subscribers are only called on state changes,
  // so we must check the current state first.
  if (router.state.initialized) {
    resolve()
    return
  }

  const unsubscribe = router.subscribe((state) => {
    if (state.initialized) {
      unsubscribe()
      resolve()
    }
  })
})

// We suspend on router initialization here, right above `RouterProvider`,
// instead of `await`ing the promise before calling `hydrateRoot`. This way
// React hydrates the rest of the tree concurrently with the route module
// download, while the commit still happens in one go once the router is
// initialized. Suspending is safe because we render inside a transition with
// no `Suspense` boundary above, so React keeps the prerendered HTML on screen
// and just delays the commit.
function InitializedRouterProvider() {
  use(routerInitialized)
  return <RouterProvider router={router} />
}

// We embed this data at prerendering time.
const { isFallbackPage } = window.__WASP_SSR_DATA__ ?? {}

function App() {
  return (
    <Layout isFallbackPage={isFallbackPage}>
      <WaspApp>
        <InitializedRouterProvider />
      </WaspApp>
    </Layout>
  );
}

startTransition(() => {
  hydrateRoot(document, <App />);
});
