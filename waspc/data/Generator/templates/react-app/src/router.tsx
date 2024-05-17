{{={= =}=}}
import React from 'react'
import { Route, Switch, BrowserRouter as Router } from 'react-router-dom'
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

const router = (
  <Router basename="{= baseDir =}">
    {=# rootComponent.isDefined =}
    <{= rootComponent.importIdentifier =}>
    {=/ rootComponent.isDefined =}
    <Switch>
      {=# isExternalAuthEnabled =}
      {/* 
        Wasp specific routes *must* go first to prevent user
        defined routes from overriding them.
        Details in https://github.com/wasp-lang/wasp/issues/2029
      */}
      <Route exact path="{= oAuthCallbackPath =}">
        <OAuthCallbackPage />
      </Route>
      {=/ isExternalAuthEnabled =}
      {Object.entries(routes).map(([routeKey, route]) => (
        <Route
          exact
          key={routeKey}
          path={route.to}
          component={routeNameToRouteComponent[routeKey]}
        />
      ))}
    </Switch>
    {=# rootComponent.isDefined =}
    </{= rootComponent.importIdentifier =}>
    {=/ rootComponent.isDefined =}
  </Router>
)

export default router
