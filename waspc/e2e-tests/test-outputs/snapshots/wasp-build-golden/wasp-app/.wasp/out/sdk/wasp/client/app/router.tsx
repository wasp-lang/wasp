import type { ReactNode } from 'react'
import type { RouteObject } from 'react-router'


import { DefaultRootErrorBoundary } from './components/DefaultRootErrorBoundary'

import { routes } from '../router/index'

type RouteMapping = Record<string, RouteObject>;

export function getRouteObjects({
  routesMapping,
  rootElement,
}: {
  routesMapping: RouteMapping,
  rootElement: ReactNode,
}): RouteObject[] {
  const waspDefinedRoutes = [
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
    children: [
      ...waspDefinedRoutes,
      ...userDefinedRoutes,
    ],
  }]
}
