import * as React from 'react'
import { createBrowserRouter, RouterProvider } from 'react-router-dom'

import { OAuthCallbackPage } from "../pages/OAuthCallback"

import { DefaultRootErrorBoundary } from '../components/DefaultRootErrorBoundary'

import { routes } from '../../router/index'

export function getRouter({
  routesMapping,
  rootElement,
}: {
  routesMapping: Record<string, React.ComponentType>,
  rootElement: React.ReactNode,
}) {
  const waspDefinedRoutes = [
    {
      path: "/oauth/callback",
      Component: OAuthCallbackPage,
    },
  ]
  const userDefinedRoutes = Object.entries(routes).map(([routeKey, route]) => {
    return {
      path: route.to,
      Component: routesMapping[routeKey],
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
    basename: '/',
  })
  return <RouterProvider router={browserRouter} />;
}
