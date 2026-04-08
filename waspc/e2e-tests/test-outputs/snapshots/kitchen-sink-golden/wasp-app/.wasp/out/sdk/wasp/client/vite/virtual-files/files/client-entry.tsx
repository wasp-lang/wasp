import { startTransition } from "react";
import { hydrateRoot } from "react-dom/client";
import { createBrowserRouter, type HydrationState } from "react-router";
import { RouterProvider } from "react-router/dom";
import { Layout } from "wasp/client/app/layout";
import { WaspApp } from "wasp/client/app";

import { routeObjects } from '/@wasp/routes.tsx'

// React Router will put hydration data on this property of the `window` object.
// https://reactrouter.com/7.13.1/start/data/custom#4-hydrate-in-the-browser
const hydrationData = (window as any).__staticRouterHydrationData as HydrationState | undefined;

const router = createBrowserRouter(routeObjects, {
  basename: "/",
  hydrationData,
})

function App({ isFallbackPage }: { isFallbackPage: boolean }) {
  return (
    <Layout isFallbackPage={isFallbackPage}>
      <WaspApp>
        <RouterProvider router={router} />
      </WaspApp>
    </Layout>
  );
}

startTransition(() => {
  const isFallbackpage = hydrationData == null;
  hydrateRoot(document, <App isFallbackPage={isFallbackpage} />);
});
