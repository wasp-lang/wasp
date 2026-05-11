import { getRouteObjects } from "wasp/client/app/router";
import { lazy } from "react"

import { createAuthRequiredPage } from "wasp/client/app"

import { App as App_ext } from './src/App'

import { EagerPage } from './src/features/lazy-loading/pages/EagerPage'

const routesMapping = {
  HomeRoute: {
    Component:
      lazy(() =>
        import('./src/pages/HomePage').then(m => m.HomePage)
        .then(component => ({ default: component }))
      ),
  },
  CatchAllRoute: {
    Component:
      lazy(() =>
        import('./src/pages/CatchAllPage').then(m => m.CatchAllPage)
        .then(component => ({ default: component }))
      ),
  },
  SignupRoute: {
    Component:
      lazy(() =>
        import('./src/features/auth/pages/Signup').then(m => m.default)
        .then(component => ({ default: component }))
      ),
  },
  LoginRoute: {
    Component:
      lazy(() =>
        import('./src/features/auth/pages/Login').then(m => m.default)
        .then(component => ({ default: component }))
      ),
  },
  PasswordResetRoute: {
    Component:
      lazy(() =>
        import('./src/features/auth/pages/PasswordReset').then(m => m.PasswordReset)
        .then(component => ({ default: component }))
      ),
  },
  EmailVerificationRoute: {
    Component:
      lazy(() =>
        import('./src/features/auth/pages/EmailVerification').then(m => m.EmailVerification)
        .then(component => ({ default: component }))
      ),
  },
  RequestPasswordResetRoute: {
    Component:
      lazy(() =>
        import('./src/features/auth/pages/RequestPasswordReset').then(m => m.RequestPasswordReset)
        .then(component => ({ default: component }))
      ),
  },
  ProfileRoute: {
    Component:
      lazy(() =>
        import('./src/features/auth/pages/ProfilePage').then(m => m.ProfilePage)
        .then(component => createAuthRequiredPage(component))
        .then(component => ({ default: component }))
      ),
  },
  ManualSignupRoute: {
    Component:
      lazy(() =>
        import('./src/features/auth/pages/ManualSignupPage').then(m => m.ManualSignupPage)
        .then(component => ({ default: component }))
      ),
  },
  CustomSignupRoute: {
    Component:
      lazy(() =>
        import('./src/features/auth/pages/CustomSignupPage').then(m => m.CustomSignupPage)
        .then(component => ({ default: component }))
      ),
  },
  TasksRoute: {
    Component:
      lazy(() =>
        import('./src/features/operations/pages/TasksPage').then(m => m.TasksPage)
        .then(component => createAuthRequiredPage(component))
        .then(component => ({ default: component }))
      ),
  },
  TaskRoute: {
    Component:
      lazy(() =>
        import('./src/features/operations/pages/TaskDetailPage').then(m => m.TaskDetailPage)
        .then(component => createAuthRequiredPage(component))
        .then(component => ({ default: component }))
      ),
  },
  SerializationRoute: {
    Component:
      lazy(() =>
        import('./src/features/operations/pages/SerializationPage').then(m => m.SerializationPage)
        .then(component => ({ default: component }))
      ),
  },
  JobsRoute: {
    Component:
      lazy(() =>
        import('./src/features/jobs/pages/JobsPage').then(m => m.JobsPage)
        .then(component => createAuthRequiredPage(component))
        .then(component => ({ default: component }))
      ),
  },
  ApisRoute: {
    Component:
      lazy(() =>
        import('./src/features/apis/pages/ApisPage').then(m => m.ApisPage)
        .then(component => ({ default: component }))
      ),
  },
  CrudListRoute: {
    Component:
      lazy(() =>
        import('./src/features/crud/pages/ListPage').then(m => m.ListPage)
        .then(component => createAuthRequiredPage(component))
        .then(component => ({ default: component }))
      ),
  },
  CrudDetailRoute: {
    Component:
      lazy(() =>
        import('./src/features/crud/pages/DetailPage').then(m => m.DetailPage)
        .then(component => createAuthRequiredPage(component))
        .then(component => ({ default: component }))
      ),
  },
  StreamingRoute: {
    Component:
      lazy(() =>
        import('./src/features/streaming/pages/StreamingTestPage').then(m => m.StreamingTestPage)
        .then(component => ({ default: component }))
      ),
  },
  ChatRoute: {
    Component:
      lazy(() =>
        import('./src/features/chat/pages/ChatPage').then(m => m.ChatPage)
        .then(component => createAuthRequiredPage(component))
        .then(component => ({ default: component }))
      ),
  },
  EagerRoute: {
    Component: EagerPage,
  },
  LazyRoute: {
    Component:
      lazy(() =>
        import('./src/features/lazy-loading/pages/LazyPage').then(m => m.LazyPage)
        .then(component => ({ default: component }))
      ),
  },
  PrerenderRoute: {
    Component:
      lazy(() =>
        import('./src/features/prerender/pages/PrerenderPage').then(m => m.PrerenderPage)
        .then(component => ({ default: component }))
      ),
  },
  HydrationMismatchRoute: {
    Component:
      lazy(() =>
        import('./src/features/prerender/pages/HydrationMismatchPage').then(m => m.HydrationMismatchPage)
        .then(component => ({ default: component }))
      ),
  },
} as const;

const rootElement =
  <App_ext />

export const routeObjects = getRouteObjects({
  routesMapping,
  rootElement,
})
