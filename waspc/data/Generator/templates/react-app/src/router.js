{{={= =}=}}
import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'

{=# pagesToImport =}
import {= importWhat =} from "{= importFrom =}"
{=/ pagesToImport =}


const router = (
  <Router>
    <div>
      {=# routes =}
      <Route exact path="{= urlPath =}" component={ {= targetPage =} }/>
      {=/ routes =}
    </div>
  </Router>
)

export default router
