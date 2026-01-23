import React from 'react'
import { createBrowserRouter, RouterProvider } from 'react-router-dom'
import { App } from '../../../../src/App'

import createAuthRequiredPage from "./auth/pages/createAuthRequiredPage"

import { HomePage } from '../../../../src/pages/HomePage'
import { CatchAllPage } from '../../../../src/pages/CatchAllPage'
import SignupPage from '../../../../src/features/auth/pages/Signup'
import LoginPage from '../../../../src/features/auth/pages/Login'
import { PasswordReset as PasswordResetPage } from '../../../../src/features/auth/pages/PasswordReset'
import { EmailVerification as EmailVerificationPage } from '../../../../src/features/auth/pages/EmailVerification'
import { RequestPasswordReset as RequestPasswordResetPage } from '../../../../src/features/auth/pages/RequestPasswordReset'
import { ProfilePage } from '../../../../src/features/auth/pages/ProfilePage'
import { ManualSignupPage } from '../../../../src/features/auth/pages/ManualSignupPage'
import { CustomSignupPage } from '../../../../src/features/auth/pages/CustomSignupPage'
import { TasksPage } from '../../../../src/features/operations/pages/TasksPage'
import { TaskDetailPage as TaskPage } from '../../../../src/features/operations/pages/TaskDetailPage'
import { SerializationPage } from '../../../../src/features/operations/pages/SerializationPage'
import { JobsPage } from '../../../../src/features/jobs/pages/JobsPage'
import { ApisPage } from '../../../../src/features/apis/pages/ApisPage'
import { ListPage as CrudList } from '../../../../src/features/crud/pages/ListPage'
import { DetailPage as CrudDetail } from '../../../../src/features/crud/pages/DetailPage'
import { StreamingTestPage as StreamingPage } from '../../../../src/features/streaming/pages/StreamingTestPage'
import { ChatPage } from '../../../../src/features/chat/pages/ChatPage'

import { OAuthCallbackPage } from "./auth/pages/OAuthCallback"

import { DefaultRootErrorBoundary } from './components/DefaultRootErrorBoundary'

import { routes } from 'wasp/client/router'

export const routeNameToRouteComponent = {
  HomeRoute: HomePage,
  CatchAllRoute: CatchAllPage,
  SignupRoute: SignupPage,
  LoginRoute: LoginPage,
  PasswordResetRoute: PasswordResetPage,
  EmailVerificationRoute: EmailVerificationPage,
  RequestPasswordResetRoute: RequestPasswordResetPage,
  ProfileRoute: createAuthRequiredPage(ProfilePage),
  ManualSignupRoute: ManualSignupPage,
  CustomSignupRoute: CustomSignupPage,
  TasksRoute: createAuthRequiredPage(TasksPage),
  TaskRoute: createAuthRequiredPage(TaskPage),
  SerializationRoute: SerializationPage,
  JobsRoute: createAuthRequiredPage(JobsPage),
  ApisRoute: ApisPage,
  CrudListRoute: createAuthRequiredPage(CrudList),
  CrudDetailRoute: createAuthRequiredPage(CrudDetail),
  StreamingRoute: StreamingPage,
  ChatRoute: createAuthRequiredPage(ChatPage),
} as const;

const waspDefinedRoutes = [
  {
    path: "/oauth/callback",
    Component: OAuthCallbackPage,
  },
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
}], {
  basename: '/',
})

export const router = <RouterProvider router={browserRouter} />
