{{={= =}=}}
import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'

import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage.js"
{=# pagesToImport =}
import {= importWhat =} from "{= importFrom =}"
{=/ pagesToImport =}


const router = (
  <Router>
    <div>
      {=# routes =}
      <Route exact path="{= urlPath =}" component={ {= targetComponent =} }/>
      {=/ routes =}
    </div>
  </Router>
)

export default router
