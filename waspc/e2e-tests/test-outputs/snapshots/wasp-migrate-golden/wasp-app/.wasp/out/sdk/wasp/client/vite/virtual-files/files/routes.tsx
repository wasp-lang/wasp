import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";
import { lazy } from "react"





const routesMapping = {
  RootRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/MainPage').then(m => m.MainPage)
        .then(component => ({ default: component }))
      ),
  },
} as const;


initializeQueryClient()

const rootElement =
  undefined

export const routeObjects = getRouteObjects({
  routesMapping,
  rootElement,
})
