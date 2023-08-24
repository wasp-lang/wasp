import React from 'react'
import { Route, Switch, BrowserRouter as Router } from 'react-router-dom'
import { interpolatePath, type ParamValue, type Search } from './router/linkHelpers'
import App from './ext-src/App.jsx'

import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage"

import MainPage from './ext-src/MainPage.jsx'

import OAuthCodeExchange from "./auth/pages/OAuthCodeExchange"

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
    <App>
    <Switch>
      <Route exact path="/" component={ MainPage }/>
      <Route exact path="/auth/login/google">
        <OAuthCodeExchange pathToApiServerRouteHandlingOauthRedirect="/auth/google/callback" />
      </Route>
    </Switch>
    </App>
  </Router>
)

export default router

export { Link } from './router/Link'
