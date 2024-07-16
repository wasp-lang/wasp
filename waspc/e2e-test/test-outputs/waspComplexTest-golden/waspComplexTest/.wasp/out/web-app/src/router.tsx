import React from 'react'
import { Route, Switch, BrowserRouter as Router } from 'react-router-dom'
import App from '../../../../src/client/App.jsx'

import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage"

import { MainPage } from '../../../../src/MainPage'

import { OAuthCallbackPage } from "./auth/pages/OAuthCallback"

import { routes } from 'wasp/client/router'

export const routeNameToRouteComponent = {
  RootRoute: MainPage,
} as const;

const router = (
  <Router basename="/">
    <App>
    <Switch>
      {/* 
        Wasp specific routes *must* go first to prevent user
        defined routes from overriding them.
        Details in https://github.com/wasp-lang/wasp/issues/2029
      */}
      <Route exact path="/oauth/callback">
        <OAuthCallbackPage />
      </Route>
      {Object.entries(routes).map(([routeKey, route]) => (
        <Route
          exact
          key={routeKey}
          path={route.to}
          component={routeNameToRouteComponent[routeKey]}
        />
      ))}
    </Switch>
    </App>
  </Router>
)

export default router
