{{={= =}=}}
import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'

{=# routes =}
import {= targetPage =} from './{= targetPage =}'
{=/ routes =}


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
