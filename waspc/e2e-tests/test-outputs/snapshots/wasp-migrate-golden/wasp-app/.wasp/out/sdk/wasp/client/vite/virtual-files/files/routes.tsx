import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";
import { lazy } from "react"





const routesMapping = {
  RootRoute: {
    Component:
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
