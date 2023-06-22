import React from 'react'
import { Route, Switch, BrowserRouter as Router } from 'react-router-dom'


import MainPage from './ext-src/MainPage.jsx'


const router = (
  <Router>
    <Switch>
      <Route exact path="/" component={ MainPage }/>
    </Switch>
  </Router>
)

export default router
