import React from 'react'
import { useRoutes, BrowserRouter } from 'react-router-dom'
import App from '../../../../src/client/App.jsx'

import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage"

import { MainPage } from '../../../../src/MainPage'

import { OAuthCallbackPage } from "./auth/pages/OAuthCallback"

import { routes } from 'wasp/client/router'

export const routeNameToRouteComponent = {
  RootRoute: MainPage,
} as const;

export function RouterRoutes() {
  const waspDefinedRoutes = [
    {
      path: "/oauth/callback",
      element: <OAuthCallbackPage />
    },
  ]
  const userDefinedRoutes = Object.entries(routes).map(([routeKey, route]) => {
    const Component = routeNameToRouteComponent[routeKey]
    return {
      path: route.to,
      element: <Component />
    }
  })
  const routerRoutes = useRoutes([
    /*
      Wasp specific routes *must* go first to prevent user
      defined routes from overriding them.
      Details in https://github.com/wasp-lang/wasp/issues/2029
    */
    ...waspDefinedRoutes,
    ...userDefinedRoutes,
  ])

  return routerRoutes
}

export const router = (
  <BrowserRouter basename="/">
    <App>
    <RouterRoutes />
    </App>
  </BrowserRouter>
)
