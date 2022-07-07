{{={= =}=}}
import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'

{=# isAuthEnabled =}
import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage.js"
{=/ isAuthEnabled =}

{=# pagesToImport =}
import {= importWhat =} from "{= importFrom =}"
{=/ pagesToImport =}

import TokenExchange from "./auth/pages/TokenExchange"

const router = (
  <Router>
    <div>
      {=# routes =}
      <Route exact path="{= urlPath =}" component={ {= targetComponent =} }/>
      {=/ routes =}

      {/* TODO: Conditionally wrap. */}
      <Route exact path="/tokenExchange" component={ TokenExchange }/>
    </div>
  </Router>
)

export default router
