{{={= =}=}}
import type { ReactNode, ComponentType } from 'react'
import { createBrowserRouter, RouterProvider, type RouteObject } from 'react-router'

{=# isExternalAuthEnabled =}
import { OAuthCallbackPage } from "../pages/OAuthCallback"
{=/ isExternalAuthEnabled =}

import { DefaultRootErrorBoundary } from '../components/DefaultRootErrorBoundary'

import { routes } from '../../router/index'

export function getRouteObjects({
  routesMapping,
  rootElement,
}: {
  routesMapping: Record<string, ComponentType>,
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
      Component: routesMapping[routeKey],
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

export function getRouter({
  routesMapping,
  rootElement,
}: {
  routesMapping: Record<string, ComponentType>,
  rootElement: ReactNode,
}) {
  const routeObjects = getRouteObjects({ routesMapping, rootElement })
  const browserRouter = createBrowserRouter(routeObjects, {
    basename: '{= baseDir =}',
  })
  return <RouterProvider router={browserRouter} />;
}
