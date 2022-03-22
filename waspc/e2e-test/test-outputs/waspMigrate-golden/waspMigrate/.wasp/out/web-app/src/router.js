import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'


import MainPage from "./ext-src/MainPage.js"


const router = (
  <Router>
    <div>
      <Route exact path="/" component={ MainPage }/>
    </div>
  </Router>
)

export default router
