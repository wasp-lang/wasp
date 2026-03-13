import * as React from 'react'
import { createBrowserRouter, RouterProvider } from 'react-router'


import { DefaultRootErrorBoundary } from '../components/DefaultRootErrorBoundary'

import { routes } from '../../router/index'

export function getRouter({
  routesMapping,
  rootElement,
}: {
  routesMapping: Record<string, { lazy: () => Promise<{ Component: React.ComponentType }> } | { Component: React.ComponentType }>,
  rootElement: React.ReactNode,
}) {
  const waspDefinedRoutes = [
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
    basename: '/',
  })
  return <RouterProvider router={browserRouter} />;
}
