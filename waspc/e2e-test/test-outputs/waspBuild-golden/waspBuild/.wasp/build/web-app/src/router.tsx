import React from 'react'
import { Route, Switch, BrowserRouter as Router } from 'react-router-dom'
import { interpolatePath, type ParamValue, type Search } from './router/linkHelpers'


import MainPage from './ext-src/MainPage.jsx'


export type Routes = 
| {to: "/", params?: {}}
| never

type OptionalRouteOptions = {
  search?: Search;
  hash?: string;
}

export const routes = {
  RootRoute: (options?: OptionalRouteOptions) => interpolatePath("/", undefined, options.search, options.hash),
}

const router = (
  <Router>
    <Switch>
      <Route exact path="/" component={ MainPage }/>
    </Switch>
  </Router>
)

export default router

export { Link } from './router/Link'
