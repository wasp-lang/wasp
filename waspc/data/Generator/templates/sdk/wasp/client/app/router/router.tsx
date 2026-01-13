{{={= =}=}}
import React from 'react'
import { createBrowserRouter, RouterProvider } from 'react-router-dom'

{=# isExternalAuthEnabled =}
import { OAuthCallbackPage } from "../pages/OAuthCallback"
{=/ isExternalAuthEnabled =}

import { DefaultRootErrorBoundary } from '../components/DefaultRootErrorBoundary'

import { routes } from '../../router/index'

export function getRouter({
  routesMapping,
  AppComponent,
}: {
  routesMapping: Record<string, React.ComponentType>,
  AppComponent: React.ComponentType,
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
      Component: routesMapping[routeKey],
    }
  })

  const browserRouter = createBrowserRouter([{
    path: '/',
    element: <AppComponent />,
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
