import React from 'react'
import { Route, Switch, BrowserRouter as Router } from 'react-router-dom'
import App__userDefined from '../../../../src/client/App.jsx'

import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage"

import { MainPage } from '../../../../src/MainPage'

import { OAuthCallbackPage } from "./auth/pages/OAuthCallback"

import { routes } from 'wasp/client/router'

export const routeNameToRouteComponent = {
  RootRoute: MainPage,
} as const;

const router = (
  <Router basename="/">
    <App__userDefined>
    <Switch>
      {Object.entries(routes).map(([routeKey, route]) => (
        <Route
          exact
          key={routeKey}
          path={route.to}
          component={routeNameToRouteComponent[routeKey]}
        />
      ))}
      <Route exact path="/oauth/callback">
        <OAuthCallbackPage />
      </Route>
    </Switch>
    </App__userDefined>
  </Router>
)

export default router
