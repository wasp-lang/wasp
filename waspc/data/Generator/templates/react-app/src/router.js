{{={= =}=}}
import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'

{=# isAuthEnabled =}
import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage.js"
{=/ isAuthEnabled =}

{=# pagesToImport =}
import {= importWhat =} from "{= importFrom =}"
{=/ pagesToImport =}

{=# isPassportRequired =}
import TokenExchange from "./auth/pages/TokenExchange"
{=/ isPassportRequired =}

const router = (
  <Router>
    <div>
      {=# routes =}
      <Route exact path="{= urlPath =}" component={ {= targetComponent =} }/>
      {=/ routes =}

      {=# isPassportRequired =}
      <Route exact path="/tokenExchange" component={ TokenExchange }/>
      {=/ isPassportRequired =}
    </div>
  </Router>
)

export default router
