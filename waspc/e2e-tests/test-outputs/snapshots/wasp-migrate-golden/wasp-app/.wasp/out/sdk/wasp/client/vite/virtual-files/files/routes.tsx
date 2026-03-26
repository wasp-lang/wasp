import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";




// These files are used from user-land and the import paths below are relative to the
// user's project dir, and not the SDK:
import { MainPage } from './src/MainPage'

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
