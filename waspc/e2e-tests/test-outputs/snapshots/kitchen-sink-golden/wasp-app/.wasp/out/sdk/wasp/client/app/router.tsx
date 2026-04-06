import type { ReactNode, ComponentType } from 'react'
import { createBrowserRouter, RouterProvider, type RouteObject } from 'react-router'

import { OAuthCallbackPage } from "./pages/OAuthCallback"

import { DefaultRootErrorBoundary } from './components/DefaultRootErrorBoundary'

import { routes } from '../router/index'
import { Fallback } from './fallback'

type RouteMapping = Record<
  string,
  | { lazy: () => Promise<{ Component: ComponentType }> }
  | { Component: ComponentType }
>;

export function getRouteObjects({
  routesMapping,
  rootElement,
}: {
  routesMapping: RouteMapping,
  rootElement: ReactNode,
}): RouteObject[] {
  const waspDefinedRoutes = [
    {
      path: "/oauth/callback",
      Component: OAuthCallbackPage,
    },
  ]
  const userDefinedRoutes = Object.entries(routes).map(([routeKey, route]) => {
    return {
      path: route.to,
      ...routesMapping[routeKey],
    }
  })

  return [{
    path: '/',
    element: rootElement,
    ErrorBoundary: DefaultRootErrorBoundary,
    HydrateFallback: Fallback,
    children: [
      ...waspDefinedRoutes,
      ...userDefinedRoutes,
    ],
  }]
}

export function getRouter({
  routesMapping,
  rootElement,
}: {
  routesMapping: RouteMapping,
  rootElement: ReactNode,
}) {
  const routeObjects = getRouteObjects({ routesMapping, rootElement })
  const browserRouter = createBrowserRouter(routeObjects, {
    basename: '/',
  })
  return <RouterProvider router={browserRouter} />;
}
