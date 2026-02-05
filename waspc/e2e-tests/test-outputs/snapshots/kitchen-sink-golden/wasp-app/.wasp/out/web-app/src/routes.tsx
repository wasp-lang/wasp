import React from 'react'
import type { RouteObject } from 'react-router'
import { routes } from 'wasp/client/router'
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

export const baseDir: string = '/'

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

export const routeNameToSsr = {
  HomeRoute: false,
  CatchAllRoute: false,
  SignupRoute: false,
  LoginRoute: false,
  PasswordResetRoute: false,
  EmailVerificationRoute: false,
  RequestPasswordResetRoute: false,
  ProfileRoute: false,
  ManualSignupRoute: false,
  CustomSignupRoute: false,
  TasksRoute: false,
  TaskRoute: false,
  SerializationRoute: false,
  JobsRoute: false,
  ApisRoute: false,
  CrudListRoute: false,
  CrudDetailRoute: false,
  StreamingRoute: false,
  ChatRoute: false,
} as const;

type RouteName = keyof typeof routeNameToRouteComponent

const waspDefinedRoutes: RouteObject[] = [
  {
    path: "/oauth/callback",
    Component: OAuthCallbackPage,
    handle: { ssr: false },
  },
]

const userDefinedRoutes: RouteObject[] = Object.entries(routes).map(([routeKey, route]) => {
  const name = routeKey as RouteName
  return {
    path: route.to,
    Component: routeNameToRouteComponent[name],
    handle: { ssr: routeNameToSsr[name] ?? false },
  }
})

export const appRoutes: RouteObject[] = [
  {
    path: '/',
    element: <App />,
    ErrorBoundary: DefaultRootErrorBoundary,
    children: [
      ...waspDefinedRoutes,
      ...userDefinedRoutes,
    ],
  },
]
