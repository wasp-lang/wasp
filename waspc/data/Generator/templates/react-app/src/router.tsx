{{={= =}=}}
import React from 'react'
import { Route, Switch, BrowserRouter as Router } from 'react-router-dom'
import { interpolatePath } from './router/linkHelpers'
import type {
  RouteDefinitionsToRoutes,
  OptionalRouteOptions,
  ParamValue,
} from './router/types'
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
import OAuthCodeExchange from "./auth/pages/OAuthCodeExchange"
{=/ isExternalAuthEnabled =}

export const routes = {
  {=# routes =}
  {= name =}: {
    to: "{= urlPath =}",
    component: {= targetComponent =},
    {=#  hasUrlParams =}
    build: (
      options: {
        params: {{=# urlParams =}{= name =}{=# isOptional =}?{=/ isOptional =}: ParamValue;{=/ urlParams =}}
      } & OptionalRouteOptions,
    ) => interpolatePath("{= urlPath =}", options.params, options.search, options.hash),
    {=/ hasUrlParams =}
    {=^ hasUrlParams =}
    build: (
      options?: OptionalRouteOptions,
    ) => interpolatePath("{= urlPath =}", undefined, options.search, options.hash),
    {=/ hasUrlParams =}
  },
  {=/ routes =}
} as const;

export type Routes = RouteDefinitionsToRoutes<typeof routes>

const router = (
  <Router>
    {=# rootComponent.isDefined =}
    <{= rootComponent.importIdentifier =}>
    {=/ rootComponent.isDefined =}
    <Switch>
      {Object.entries(routes).map(([routeKey, route]) => (
        <Route
          exact
          key={routeKey}
          path={route.to}
          component={route.component}
        />
      ))}
      {=# isExternalAuthEnabled =}
      {=# externalAuthProviders =}
      {=# authProviderEnabled =}
      <Route exact path="{= authFrontendUrl =}">
        <OAuthCodeExchange pathToApiServerRouteHandlingOauthRedirect="{= authServerOauthRedirectUrl =}" />
      </Route>
      {=/ authProviderEnabled =}
      {=/ externalAuthProviders =}
      {=/ isExternalAuthEnabled =}
    </Switch>
    {=# rootComponent.isDefined =}
    </{= rootComponent.importIdentifier =}>
    {=/ rootComponent.isDefined =}
  </Router>
)

export default router

export { Link } from './router/Link'
