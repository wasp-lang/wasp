{{={= =}=}}
import React from 'react'
import { createBrowserRouter, RouterProvider } from 'react-router-dom'
{=# rootComponent.isDefined =}
{=& rootComponent.importStatement =}
{=/ rootComponent.isDefined =}

{=# isAuthEnabled =}
import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage"
{=/ isAuthEnabled =}

{=# isExternalAuthEnabled =}
import { OAuthCallbackPage } from "./auth/pages/OAuthCallback"
{=/ isExternalAuthEnabled =}

import { routes } from 'wasp/client/router'

export const routeNameToRouteComponent = {
  {=# routes =}
  {= name =}: async () => {
    {=& importExpr =}
    {=# isAuthRequired =}return { Component: createAuthRequiredPage({= targetComponent =}) }{=/ isAuthRequired =}
    {=^ isAuthRequired =}return { Component: {= targetComponent =} }{=/ isAuthRequired =}
  },
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
    lazy: routeNameToRouteComponent[routeKey],
  }
})

const browserRouter = createBrowserRouter([{
  path: '/',
  {=# rootComponent.isDefined =}
  element: <{= rootComponent.importIdentifier =} />,
  {=/ rootComponent.isDefined =}
  children: [
    ...waspDefinedRoutes,
    ...userDefinedRoutes,
  ],
}])


export const router = <RouterProvider router={browserRouter} />
