{{={= =}=}}
import React from 'react'
import { Route, Routes, BrowserRouter as Router } from 'react-router-dom'

{=# isAuthEnabled =}
import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage.js"
{=/ isAuthEnabled =}

{=# pagesToImport =}
import {= importWhat =} from "{= importFrom =}"
{=/ pagesToImport =}

{=# routes =}
{=# authRequired =}
const {= targetComponent =}Component = createAuthRequiredPage({= targetComponent =})
{=/ authRequired =}
{=^ authRequired =}
const {= targetComponent =}Component = {= targetComponent =}
{=/ authRequired =}
{=/ routes =}

const router = (
  <Router>
    <Routes>
      {=# routes =}
      <Route exact path="{= urlPath =}" element={ <{= targetComponent =}Component /> }/>
      {=/ routes =}
    </Routes>
  </Router>
)

export default router
