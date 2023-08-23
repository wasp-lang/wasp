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

export type Routes = 
  {=# routes =}
{=# hasUrlParams =}
| {to: "{= urlPath =}", params: {{=# urlParams =}{= name =}{=# isOptional =}?{=/ isOptional =}: ParamValue;{=/ urlParams =}}}
{=/ hasUrlParams =}
{=^ hasUrlParams =}
| {to: "{= urlPath =}", params?: {}}
{=/ hasUrlParams =}
  {=/ routes =}
| never

type OptionalRouteOptions = {
  search?: Search;
  hash?: string;
}

export const routes = {
  {=# routes =}
  {=# hasUrlParams =}
  {= name =}: (options: { params: {{=# urlParams =}{= name =}{=# isOptional =}?{=/ isOptional =}: ParamValue;{=/ urlParams =}} } & OptionalRouteOptions) => interpolatePath("{= urlPath =}", options.params, options.search, options.hash),
  {=/ hasUrlParams =}
  {=^ hasUrlParams =}
  {= name =}: (options?: OptionalRouteOptions) => interpolatePath("{= urlPath =}", undefined, options.search, options.hash),
  {=/ hasUrlParams =}
  {=/ routes =}
}

const router = (
  <Router>
    {=# rootComponent.isDefined =}
    <{= rootComponent.importIdentifier =}>
    {=/ rootComponent.isDefined =}
    <Switch>
      {=# routes =}
      <Route exact path="{= urlPath =}" component={ {= targetComponent =} }/>
      {=/ routes =}
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
