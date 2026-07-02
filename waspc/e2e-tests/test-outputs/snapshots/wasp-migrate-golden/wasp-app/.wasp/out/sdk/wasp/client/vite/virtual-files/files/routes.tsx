import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";





const routesMapping = {
  RootRoute: {
    lazy: () =>
      import('./src/MainPage').then(m => m.MainPage)
      .then(component => ({ Component: component })),
  },
} as const;


initializeQueryClient()

const rootElement =
  undefined

export const routeObjects = getRouteObjects({
  routesMapping,
  rootElement,
})
