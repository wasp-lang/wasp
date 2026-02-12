{{={= =}=}}
import * as React from 'react'
import { createBrowserRouter, RouterProvider } from 'react-router'

{=# isExternalAuthEnabled =}
import { OAuthCallbackPage } from "../pages/OAuthCallback"
{=/ isExternalAuthEnabled =}

import { DefaultRootErrorBoundary } from '../components/DefaultRootErrorBoundary'

import { routes } from '../../router/index'

export function getRouter({
  routesMapping,
  rootElement,
  routeNameToSsr,
}: {
  routesMapping: Record<string, React.ComponentType>,
  rootElement: React.ReactNode,
  routeNameToSsr: Record<string, boolean>,
}) {
  const waspDefinedRoutes = [
    {=# isExternalAuthEnabled =}
    {
      path: "{= oAuthCallbackPath =}",
      Component: OAuthCallbackPage,
      handle: { ssr: false },
    },
    {=/ isExternalAuthEnabled =}
  ]
  const userDefinedRoutes = Object.entries(routes).map(([routeKey, route]) => {
    return {
      path: route.to,
      Component: routesMapping[routeKey],
      handle: { ssr: routeNameToSsr[routeKey] ?? false },
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
