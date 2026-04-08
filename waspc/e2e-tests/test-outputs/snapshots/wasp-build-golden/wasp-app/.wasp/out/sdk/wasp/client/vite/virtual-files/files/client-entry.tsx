import { startTransition } from "react";
import { hydrateRoot } from "react-dom/client";
import { createBrowserRouter } from "react-router";
import { RouterProvider } from "react-router/dom";
import { Layout } from "wasp/client/app/layout";

import { routeObjects } from '/@wasp/routes.tsx'

// We embed this data at prerendering time
const { isFallbackPage } = (window as any).__WASP_SSR_DATA__ ?? {}

const router = createBrowserRouter(routeObjects, {
  basename: "/",
  // React Router will put hydration data on this property of the `window` object.
  // https://reactrouter.com/7.13.1/start/data/custom#4-hydrate-in-the-browser
  hydrationData: (window as any).__staticRouterHydrationData,
})

function App() {
  return (
    <Layout isFallbackPage={isFallbackPage}>
      <RouterProvider router={router} />
    </Layout>
  );
}

startTransition(() => {
  hydrateRoot(document, <App />);
});
