{{={= =}=}}
import { startTransition } from "react";
import { createRoot, hydrateRoot } from "react-dom/client";
import { createBrowserRouter } from "react-router";
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

// We embed this data at prerendering time.
const { isFallbackPage } = window.__WASP_SSR_DATA__ ?? {}

const app =
  waitForRouterInitialized(router).then(() => (
    <RouterProvider router={router} />
  ))

const tree = (
  <Layout>
    <WaspApp>
      {app}
    </WaspApp>
  </Layout>
)

startTransition(() => {
  if (isFallbackPage) {
    createRoot(document).render(tree);
  } else {
    hydrateRoot(document, tree);
  }
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
