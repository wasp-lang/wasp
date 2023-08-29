{{={= =}=}}
import React from 'react'
import { Route, Switch, BrowserRouter as Router } from 'react-router-dom'
import { interpolatePath, type ParamValue, type Search } from './router/linkHelpers'
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
    build: (options: { params: {{=# urlParams =}{= name =}{=# isOptional =}?{=/ isOptional =}: ParamValue;{=/ urlParams =}} } & OptionalRouteOptions) => interpolatePath("{= urlPath =}", options.params, options.search, options.hash),
    {=/ hasUrlParams =}
    {=^ hasUrlParams =}
    build: (options?: OptionalRouteOptions) => interpolatePath("{= urlPath =}", undefined, options.search, options.hash),
    {=/ hasUrlParams =}
  },
  {=/ routes =}
} as const;

type OptionalRouteOptions = {
  search?: Search;
  hash?: string;
};

type RoutesInternal = typeof routes;

type RouteToParams = {
  [K in keyof RoutesInternal]: {
    to: RoutesInternal[K]["to"];
  } & (Parameters<RoutesInternal[K]["build"]>[0] extends {
    params: infer Params;
  }
    ? { params: Params }
    : { params?: never });
};

export type Routes = RouteToParams[keyof RouteToParams];

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
