{{={= =}=}}
import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'
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

const router = (
  <Router>
    {=# rootComponent.isDefined =}
    <{= rootComponent.importIdentifier =}>
    {=/ rootComponent.isDefined =}
    {=^ rootComponent.isDefined =}
    <>
    {=/ rootComponent.isDefined =}
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
    {=# rootComponent.isDefined =}
    </{= rootComponent.importIdentifier =}>
    {=/ rootComponent.isDefined =}
    {=^ rootComponent.isDefined =}
    </>
    {=/ rootComponent.isDefined =}
  </Router>
)

export default router
