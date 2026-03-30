import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";





const routesMapping = {
  RootRoute: { lazy: async () => {
    const Component = await import('./src/MainPage').then(m => m.MainPage)
    return { Component }
  }},
} as const;


initializeQueryClient()

const rootElement =
  undefined

export const routeObjects = getRouteObjects({
  routesMapping,
  rootElement,
})
