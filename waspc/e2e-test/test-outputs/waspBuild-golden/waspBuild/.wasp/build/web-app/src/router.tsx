import React from 'react'
import { useRoutes, BrowserRouter } from 'react-router-dom'


import { MainPage } from '../../../../src/MainPage'


import { routes } from 'wasp/client/router'

export const routeNameToRouteComponent = {
  RootRoute: MainPage,
} as const;

export function RouterRoutes() {
  const routerRoutes = useRoutes([
  ...(Object.entries(routes).map(([routeKey, route]) => {
    const Component = routeNameToRouteComponent[routeKey]
    return {
      path: route.to,
      element: <Component />
    }
  }))
  ])

  return routerRoutes
}

export const router = (
  <BrowserRouter basename="/">
      <RouterRoutes />
    </BrowserRouter>
)
