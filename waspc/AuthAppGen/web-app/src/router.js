import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'

import Main from "./ext-src/MainPage.js"


const router = (
  <Router>
    <div>
      <Route exact path="/" component={ Main }/>
    </div>
  </Router>
)

export default router
