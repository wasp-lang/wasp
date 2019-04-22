{{={= =}=}}
import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'

import App from './App'
{=# pages =}
import {= name =} from './{= name =}'
{=/ pages =}


const router = (
  <Router>
    <div>
      <Route exact path="/" component={App} />
      {=# pages =}
      <Route exact path="{= route =}" component={ {= name =} }/>
      {=/ pages =}
    </div>
  </Router>
)

export default router
