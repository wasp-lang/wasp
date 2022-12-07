{{={= =}=}}
import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'

{=# isAuthEnabled =}
import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage.js"
{=/ isAuthEnabled =}

{=# pagesToImport =}
import {= importWhat =} from "{= importFrom =}"
{=/ pagesToImport =}

{=# isExternalAuthEnabled =}
import OAuthCodeExchange from "./auth/pages/OAuthCodeExchange"
{=/ isExternalAuthEnabled =}

const router = (
  <Router>
    <div>
      {=# routes =}
      <Route exact path="{= urlPath =}" component={ {= targetComponent =} }/>
      {=/ routes =}

      {=# isExternalAuthEnabled =}
      {=# externalAuthProviders =}
      {=# authProviderEnabled =}
      <Route exact path="/auth/login/{= authProviderName =}">
        <OAuthCodeExchange pathToApiServerRouteHandlingOauthRedirect="/auth/external/{= authProviderName =}/validateCodeForLogin" />
      </Route>
      {=/ authProviderEnabled =}
      {=/ externalAuthProviders =}
      {=/ isExternalAuthEnabled =}
    </div>
  </Router>
)

export default router
