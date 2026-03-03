{{={= =}=}}
// @ts-nocheck
import { startTransition } from "react";
import { createRoot, hydrateRoot } from "react-dom/client";
import { createBrowserRouter } from "react-router";
import { RouterProvider } from "react-router/dom";
import { QueryClientProvider } from "@tanstack/react-query";
import { Layout } from "wasp/client/app/layout";
import { getRouteObjects } from "wasp/client/app/router/router";
import { initializeQueryClient, queryClientInitialized } from "wasp/client/operations";
{=!
  // NOTE: We are not inlining routes mapping into this file because once we
  // allow users to override the `index.tsx` entry point they can use the existing
  // routes mapping.
=}
{=& routesMapping.importStatement =}

{=# rootComponent.isDefined =}
{=& rootComponent.importStatement =}
{=/ rootComponent.isDefined =}

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
{=/ setupFn.isDefined =}

{=# setupFn.isDefined =}
await {= setupFn.importIdentifier =}()
{=/ setupFn.isDefined =}

initializeQueryClient()

const rootElement =
  {=# rootComponent.isDefined =}
  <{= rootComponent.importIdentifier =} />
  {=/ rootComponent.isDefined =}
  {=^ rootComponent.isDefined =}
  undefined
  {=/ rootComponent.isDefined =}

const routeObjects = getRouteObjects({
  routesMapping: {= routesMapping.importIdentifier =},
  rootElement,
})

const router = createBrowserRouter(routeObjects, {
  basename: "{= baseDir =}",
  hydrationData: window.__staticRouterHydrationData,
})

const queryClient = await queryClientInitialized

function App({ isFallbackPage }: { isFallbackPage: boolean }) {
  return (
    <Layout isFallbackPage={isFallbackPage}>
      <QueryClientProvider client={queryClient}>
        <RouterProvider router={router} />
      </QueryClientProvider>
    </Layout>
  );
}

startTransition(() => {
  if (window.__staticRouterHydrationData) {
    hydrateRoot(document, <App isFallbackPage={false} />);
  } else {
    createRoot(document).render(<App isFallbackPage={true} />);
  }
});
