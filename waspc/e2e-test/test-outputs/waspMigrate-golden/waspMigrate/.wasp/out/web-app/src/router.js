import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'


import MainPage from './ext-src/MainPage'


const router = (
  <Router>
    <>
      <Route exact path="/" component={ MainPage }/>
    </>
  </Router>
)

export default router
