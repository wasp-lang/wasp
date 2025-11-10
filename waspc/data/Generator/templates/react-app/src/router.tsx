{{={= =}=}}
import React from 'react'
import { createBrowserRouter, RouterProvider } from 'react-router-dom'
{=# rootComponent.isDefined =}
{=& rootComponent.importStatement =}
{=/ rootComponent.isDefined =}

{=# isAuthEnabled =}
import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage"
{=/ isAuthEnabled =}

{=# pagesToImport =}
{=& importStatement =}
{=/ pagesToImport =}

{=# isExternalAuthEnabled =}
import { OAuthCallbackPage } from "./auth/pages/OAuthCallback"
{=/ isExternalAuthEnabled =}

import { DefaultRootErrorBoundary } from './components/DefaultRootErrorBoundary'

import { routes } from 'wasp/client/router'

export const routeNameToRouteComponent = {
  {=# routes =}
  {= name =}: {= targetComponent =},
  {=/ routes =}
} as const;

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
    Component: routeNameToRouteComponent[routeKey],
  }
})

const browserRouter = createBrowserRouter([{
  path: '/',
  {=# rootComponent.isDefined =}
  element: <{= rootComponent.importIdentifier =} />,
  {=/ rootComponent.isDefined =}
  ErrorBoundary: DefaultRootErrorBoundary,
  children: [
    ...waspDefinedRoutes,
    ...userDefinedRoutes,
  ],
}], {
  basename: '{= baseDir =}',
})

export const router = <RouterProvider router={browserRouter} />
