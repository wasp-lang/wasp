import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'

import Main from './Main'
import About from './About'


const router = (
  <Router>
    <div>
      <Route exact path="/" component={ Main }/>
      <Route exact path="/about" component={ About }/>
    </div>
  </Router>
)

export default router
