import { getRouteObjects } from "wasp/client/app/router";
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

const rootElement =
  undefined

export const routeObjects = getRouteObjects({
  routesMapping,
  rootElement,
})
