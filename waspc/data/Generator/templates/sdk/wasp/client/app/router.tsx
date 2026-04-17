{{={= =}=}}
import type { ReactNode, ComponentType } from 'react'
import type { RouteObject } from 'react-router'

{=# isExternalAuthEnabled =}
import { OAuthCallbackPage } from "./pages/OAuthCallback"
{=/ isExternalAuthEnabled =}

import { DefaultRootErrorBoundary } from './components/DefaultRootErrorBoundary'

import { routes } from '../router/index'

type RouteMapping = Record<
  string,
  { Component: ComponentType }
>;

export function getRouteObjects({
  routesMapping,
  rootElement,
}: {
  routesMapping: RouteMapping,
  rootElement: ReactNode,
}): RouteObject[] {
  const waspDefinedRoutes = [
    {=# isExternalAuthEnabled =}
    {
      path: "{= oAuthCallbackPath =}",
      Component: OAuthCallbackPage,
    },
    {=/ isExternalAuthEnabled =}
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
