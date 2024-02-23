import React from 'react'
import { Route, Switch, BrowserRouter as Router } from 'react-router-dom'


import { MainPage } from '../../../../src/MainPage'


import { routes } from 'wasp/client/router'

export const routeNameToRouteComponent = {
  RootRoute: MainPage,
} as const;

const router = (
  <Router basename="/">
    <Switch>
      {Object.entries(routes).map(([routeKey, route]) => (
        <Route
          exact
          key={routeKey}
          path={route.to}
          component={routeNameToRouteComponent[routeKey]}
        />
      ))}
    </Switch>
  </Router>
)

export default router
