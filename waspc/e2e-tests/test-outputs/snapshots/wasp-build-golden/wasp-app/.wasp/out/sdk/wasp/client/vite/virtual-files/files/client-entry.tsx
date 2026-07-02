import { startTransition } from "react";
import { createRoot, hydrateRoot } from "react-dom/client";
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

// We embed this data at prerendering time. If it's missing, we fall back to the
// hydrate branch below, which matches the previous behavior and is safe for
// prerendered pages.
const { isFallbackPage } = window.__WASP_SSR_DATA__ ?? {}

function App() {
  return (
    // On the client we always render the page content, so we don't pass
    // `isFallbackPage` to the `Layout`.
    <Layout>
      <WaspApp>
        <RouterProvider router={router} />
      </WaspApp>
    </Layout>
  );
}

startTransition(() => {
  if (isFallbackPage) {
    // The fallback shell has no prerendered content, so mount fresh instead of
    // hydrating.
    createRoot(document).render(<App />);
  } else {
    hydrateRoot(document, <App />);
  }
});
