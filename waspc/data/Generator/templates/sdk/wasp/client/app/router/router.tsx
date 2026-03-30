{{={= =}=}}
import * as React from 'react'
import { createBrowserRouter, RouterProvider } from 'react-router'

{=# isExternalAuthEnabled =}
import { OAuthCallbackPage } from "../pages/OAuthCallback"
{=/ isExternalAuthEnabled =}

import { DefaultRootErrorBoundary } from '../components/DefaultRootErrorBoundary'

import type { RouteMapping } from '../components/WaspApp'
import { routes } from '../../router/index'

export function getRouter({
  routesMapping,
  rootElement,
}: {
  routesMapping: RouteMapping,
  rootElement: React.ReactNode,
}) {
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

  const browserRouter = createBrowserRouter([{
    path: '/',
    element: rootElement,
    ErrorBoundary: DefaultRootErrorBoundary,
    children: [
      ...waspDefinedRoutes,
      ...userDefinedRoutes,
    ],
  }], {
    basename: '{= baseDir =}',
  })
  return <RouterProvider router={browserRouter} />;
}
