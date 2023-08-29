import React from 'react'
import { Route, Switch, BrowserRouter as Router } from 'react-router-dom'
import { interpolatePath } from './router/linkHelpers'
import type {
  RouteDefinitionsToRoutes,
  OptionalRouteOptions,
  ParamValue,
} from './router/types'


import MainPage from './ext-src/MainPage.jsx'


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
    <Switch>
      {Object.entries(routes).map(([routeKey, route]) => (
        <Route
          exact
          key={routeKey}
          path={route.to}
          component={route.component}
        />
      ))}
    </Switch>
  </Router>
)

export default router

export { Link } from './router/Link'
