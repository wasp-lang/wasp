{{={= =}=}}
import React from 'react'
import { useRoutes, BrowserRouter } from 'react-router-dom'
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

import { routes } from 'wasp/client/router'

export const routeNameToRouteComponent = {
  {=# routes =}
  {= name =}: {= targetComponent =},
  {=/ routes =}
} as const;

export function RouterRoutes() {
  const waspDefinedRoutes = [
    {=# isExternalAuthEnabled =}
    {
      path: "{= oAuthCallbackPath =}",
      element: <OAuthCallbackPage />
    },
    {=/ isExternalAuthEnabled =}
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
  <BrowserRouter basename="{= baseDir =}">
    {=# rootComponent.isDefined =}
    <{= rootComponent.importIdentifier =}>
    {=/ rootComponent.isDefined =}
    <RouterRoutes />
    {=# rootComponent.isDefined =}
    </{= rootComponent.importIdentifier =}>
    {=/ rootComponent.isDefined =}
  </BrowserRouter>
)
