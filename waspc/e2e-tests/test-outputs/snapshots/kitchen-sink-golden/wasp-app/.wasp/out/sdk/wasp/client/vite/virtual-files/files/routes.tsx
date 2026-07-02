import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";

import { createAuthRequiredPage } from "wasp/client/app"

import { App as App_ext } from './src/App'

import { clientSetup as clientSetup_ext } from './src/clientSetup'

import { EagerPage } from './src/features/lazy-loading/pages/EagerPage'

const routesMapping = {
  HomeRoute: {
    lazy: () =>
      import('./src/pages/HomePage').then(m => m.HomePage)
      .then(component => ({ Component: component })),
  },
  CatchAllRoute: {
    lazy: () =>
      import('./src/pages/CatchAllPage').then(m => m.CatchAllPage)
      .then(component => ({ Component: component })),
  },
  SignupRoute: {
    lazy: () =>
      import('./src/features/auth/pages/Signup').then(m => m.default)
      .then(component => ({ Component: component })),
  },
  LoginRoute: {
    lazy: () =>
      import('./src/features/auth/pages/Login').then(m => m.default)
      .then(component => ({ Component: component })),
  },
  PasswordResetRoute: {
    lazy: () =>
      import('./src/features/auth/pages/PasswordReset').then(m => m.PasswordReset)
      .then(component => ({ Component: component })),
  },
  EmailVerificationRoute: {
    lazy: () =>
      import('./src/features/auth/pages/EmailVerification').then(m => m.EmailVerification)
      .then(component => ({ Component: component })),
  },
  RequestPasswordResetRoute: {
    lazy: () =>
      import('./src/features/auth/pages/RequestPasswordReset').then(m => m.RequestPasswordReset)
      .then(component => ({ Component: component })),
  },
  ProfileRoute: {
    lazy: () =>
      import('./src/features/auth/pages/ProfilePage').then(m => m.ProfilePage)
      .then(component => createAuthRequiredPage(component))
      .then(component => ({ Component: component })),
  },
  ManualSignupRoute: {
    lazy: () =>
      import('./src/features/auth/pages/ManualSignupPage').then(m => m.ManualSignupPage)
      .then(component => ({ Component: component })),
  },
  CustomSignupRoute: {
    lazy: () =>
      import('./src/features/auth/pages/CustomSignupPage').then(m => m.CustomSignupPage)
      .then(component => ({ Component: component })),
  },
  TasksRoute: {
    lazy: () =>
      import('./src/features/operations/pages/TasksPage').then(m => m.TasksPage)
      .then(component => createAuthRequiredPage(component))
      .then(component => ({ Component: component })),
  },
  TaskRoute: {
    lazy: () =>
      import('./src/features/operations/pages/TaskDetailPage').then(m => m.TaskDetailPage)
      .then(component => createAuthRequiredPage(component))
      .then(component => ({ Component: component })),
  },
  SerializationRoute: {
    lazy: () =>
      import('./src/features/operations/pages/SerializationPage').then(m => m.SerializationPage)
      .then(component => ({ Component: component })),
  },
  JobsRoute: {
    lazy: () =>
      import('./src/features/jobs/pages/JobsPage').then(m => m.JobsPage)
      .then(component => createAuthRequiredPage(component))
      .then(component => ({ Component: component })),
  },
  ApisRoute: {
    lazy: () =>
      import('./src/features/apis/pages/ApisPage').then(m => m.ApisPage)
      .then(component => ({ Component: component })),
  },
  CrudListRoute: {
    lazy: () =>
      import('./src/features/crud/pages/ListPage').then(m => m.ListPage)
      .then(component => createAuthRequiredPage(component))
      .then(component => ({ Component: component })),
  },
  CrudDetailRoute: {
    lazy: () =>
      import('./src/features/crud/pages/DetailPage').then(m => m.DetailPage)
      .then(component => createAuthRequiredPage(component))
      .then(component => ({ Component: component })),
  },
  StreamingRoute: {
    lazy: () =>
      import('./src/features/streaming/pages/StreamingTestPage').then(m => m.StreamingTestPage)
      .then(component => ({ Component: component })),
  },
  ChatRoute: {
    lazy: () =>
      import('./src/features/chat/pages/ChatPage').then(m => m.ChatPage)
      .then(component => createAuthRequiredPage(component))
      .then(component => ({ Component: component })),
  },
  EagerRoute: {
    Component: EagerPage,
  },
  LazyRoute: {
    lazy: () =>
      import('./src/features/lazy-loading/pages/LazyPage').then(m => m.LazyPage)
      .then(component => ({ Component: component })),
  },
  PrerenderRoute: {
    lazy: () =>
      import('./src/features/prerender/pages/PrerenderPage').then(m => m.PrerenderPage)
      .then(component => ({ Component: component })),
  },
  PrerenderInstancesRoute: {
    lazy: () =>
      import('./src/features/prerender/pages/PrerenderInstancesPage').then(m => m.PrerenderInstancesPage)
      .then(component => ({ Component: component })),
  },
  HydrationMismatchRoute: {
    lazy: () =>
      import('./src/features/prerender/pages/HydrationMismatchPage').then(m => m.HydrationMismatchPage)
      .then(component => ({ Component: component })),
  },
} as const;

await clientSetup_ext()

initializeQueryClient()

const rootElement =
  <App_ext />

export const routeObjects = getRouteObjects({
  routesMapping,
  rootElement,
})
