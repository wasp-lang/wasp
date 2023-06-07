{{={= =}=}}
import React, { useMemo } from 'react'
import { Route, Switch, BrowserRouter as Router, Link as RouterLink } from 'react-router-dom'
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

type Routes = 
  {=# routes =}
| {to: "{= urlPath =}", params{=^ urlParams =}?{=/ urlParams =}: {{=# urlParams =} {= . =}: string | number;{=/ urlParams =} }}
  {=/ routes =}
| never

export function Link({ to, params, children }: Routes & { children: React.ReactNode }) {
  const toWithParams = useMemo(() => {
    return Object.keys(params).reduce((acc, key) => acc.replace(`:${key}`, params[key]), to)
  }, [to, params])
  return <RouterLink to={toWithParams}>{children}</RouterLink>
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
