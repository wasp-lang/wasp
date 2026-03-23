import * as React from 'react'
import { createBrowserRouter, RouterProvider } from 'react-router'


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
