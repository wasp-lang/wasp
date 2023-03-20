import React from 'react'
import { Route, BrowserRouter as Router } from 'react-router-dom'
import App from './ext-src/App.jsx'

import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage"

import MainPage from './ext-src/MainPage.jsx'

import OAuthCodeExchange from "./auth/pages/OAuthCodeExchange"

const router = (
  <Router>
    <App>
      <Route exact path="/" component={ MainPage }/>
      <Route exact path="/auth/login/google">
        <OAuthCodeExchange pathToApiServerRouteHandlingOauthRedirect="/auth/google/callback" />
      </Route>
    </App>
  </Router>
)

export default router
