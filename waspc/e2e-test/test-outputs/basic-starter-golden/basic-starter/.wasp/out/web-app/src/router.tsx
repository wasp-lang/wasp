import React from 'react'
import { createBrowserRouter, RouterProvider } from 'react-router-dom'
import { App } from '../../../../src/App.tsx'

import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage"

import { LoginPage } from '../../../../src/auth/email/LoginPage.tsx'
import { SignupPage } from '../../../../src/auth/email/SignupPage.tsx'
import { RequestPasswordResetPage } from '../../../../src/auth/email/RequestPasswordResetPage.tsx'
import { PasswordResetPage } from '../../../../src/auth/email/PasswordResetPage.tsx'
import { EmailVerificationPage } from '../../../../src/auth/email/EmailVerificationPage.tsx'
import { TasksPage } from '../../../../src/tasks/TasksPage.tsx'


import { DefaultRootErrorBoundary } from './components/DefaultRootErrorBoundary'

import { routes } from 'wasp/client/router'

export const routeNameToRouteComponent = {
  LoginRoute: LoginPage,
  SignupRoute: SignupPage,
  RequestPasswordResetRoute: RequestPasswordResetPage,
  PasswordResetRoute: PasswordResetPage,
  EmailVerificationRoute: EmailVerificationPage,
  TasksRoute: createAuthRequiredPage(TasksPage),
} as const;

const waspDefinedRoutes = [
]
const userDefinedRoutes = Object.entries(routes).map(([routeKey, route]) => {
  return {
    path: route.to,
    Component: routeNameToRouteComponent[routeKey],
  }
})

const browserRouter = createBrowserRouter([{
  path: '/',
  element: <App />,
  ErrorBoundary: DefaultRootErrorBoundary,
  children: [
    ...waspDefinedRoutes,
    ...userDefinedRoutes,
  ],
}])

export const router = <RouterProvider router={browserRouter} />
