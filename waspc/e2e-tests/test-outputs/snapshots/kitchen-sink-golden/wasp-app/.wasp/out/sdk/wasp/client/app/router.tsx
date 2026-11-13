import type { ReactNode } from 'react'
import type { RouteObject } from 'react-router'

import { OAuthCallbackPage } from "./pages/OAuthCallback"

import { DefaultRootErrorBoundary } from '@wasp/lib-sdk-core/browser'

import { routes } from '../router/index'

type RouteMapping = Record<string, RouteObject>;

export function getRouteObjects({
  routesMapping,
  rootElement,
}: {
  routesMapping: RouteMapping,
  rootElement: ReactNode,
}): RouteObject[] {
  const waspDefinedRoutes: RouteObject[] = [
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
    children: [
      ...waspDefinedRoutes,
      ...userDefinedRoutes,
    ],
  }]
}
