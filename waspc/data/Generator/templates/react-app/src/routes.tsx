{{={= =}=}}
import React from 'react'
import type { RouteObject } from 'react-router'
import { routes } from 'wasp/client/router'
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

export const baseDir: string = '{= baseDir =}'

export const routeNameToRouteComponent = {
  {=# routes =}
  {= name =}: {= targetComponent =},
  {=/ routes =}
} as const;

export const routeNameToSsr = {
  {=# routes =}
  {= name =}: {=# ssr =}true{=/ ssr =}{=^ ssr =}false{=/ ssr =},
  {=/ routes =}
} as const;

type RouteName = keyof typeof routeNameToRouteComponent

const waspDefinedRoutes: RouteObject[] = [
  {=# isExternalAuthEnabled =}
  {
    path: "{= oAuthCallbackPath =}",
    Component: OAuthCallbackPage,
    handle: { ssr: false },
  },
  {=/ isExternalAuthEnabled =}
]

const userDefinedRoutes: RouteObject[] = Object.entries(routes).map(([routeKey, route]) => {
  const name = routeKey as RouteName
  return {
    path: route.to,
    Component: routeNameToRouteComponent[name],
    handle: { ssr: routeNameToSsr[name] ?? false },
  }
})

export const appRoutes: RouteObject[] = [
  {
    path: '/',
    {=# rootComponent.isDefined =}
    element: <{= rootComponent.importIdentifier =} />,
    {=/ rootComponent.isDefined =}
    ErrorBoundary: DefaultRootErrorBoundary,
    children: [
      ...waspDefinedRoutes,
      ...userDefinedRoutes,
    ],
  },
]
