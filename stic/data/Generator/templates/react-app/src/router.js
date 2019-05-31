{{={= =}=}}
import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'

{=# pages =}
import {= name =} from './{= name =}'
{=/ pages =}


const router = (
  <Router>
    <div>
      {=# pages =}
      <Route exact path="{= route =}" component={ {= name =} }/>
      {=/ pages =}
    </div>
  </Router>
)

export default router
