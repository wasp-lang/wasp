import React from 'react'
import { Route, Switch, BrowserRouter as Router } from 'react-router-dom'
import { interpolatePath } from './router/linkHelpers'
import type {
  RouteDefinitionsToRoutes,
  OptionalRouteOptions,
  ParamValue,
} from './router/types'
import App from './ext-src/App.jsx'

import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage"

import MainPage from './ext-src/MainPage.jsx'

import OAuthCodeExchange from "./auth/pages/OAuthCodeExchange"

export const routes = {
  RootRoute: {
    to: "/",
    component: MainPage,
    build: (
      options?: OptionalRouteOptions,
    ) => interpolatePath("/", undefined, options.search, options.hash),
  },
} as const;

export type Routes = RouteDefinitionsToRoutes<typeof routes>

const router = (
  <Router>
    <App>
    <Switch>
      {Object.entries(routes).map(([routeKey, route]) => (
        <Route
          exact
          key={routeKey}
          path={route.to}
          component={route.component}
        />
      ))}
      <Route exact path="/auth/login/google">
        <OAuthCodeExchange pathToApiServerRouteHandlingOauthRedirect="/auth/google/callback" />
      </Route>
    </Switch>
    </App>
  </Router>
)

export default router

export { Link } from './router/Link'
