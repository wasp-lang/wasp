import React, { useMemo } from 'react'
import { Route, Switch, BrowserRouter as Router, Link as RouterLink } from 'react-router-dom'
import App from './ext-src/App.jsx'

import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage"

import MainPage from './ext-src/MainPage.jsx'

import OAuthCodeExchange from "./auth/pages/OAuthCodeExchange"

type Routes = 
| {to: "/", params?: { }}
| never

type RouterLinkProps = Parameters<typeof RouterLink>[0]

export function Link({ to, params, children, ...restOfProps }: RouterLinkProps & Routes) {
  const toWithParams = useMemo(() => {
    return to.split('/').map((part) => {
      if (part.startsWith(':')) {
        const paramName = part.slice(1)
        return params[paramName]
      }
      return part
    }).join('/')
  }, [to, params])
  return <RouterLink to={toWithParams} {...restOfProps}>{children}</RouterLink>
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
