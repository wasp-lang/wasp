import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";
import { lazy } from "react"

import { createAuthRequiredPage } from "wasp/client/app"

import { App as App_ext } from './src/App'

import { setup as setup_ext } from './src/clientSetup'

import { EagerPage } from './src/features/lazy-loading/pages/EagerPage'

const routesMapping = {
  HomeRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/pages/HomePage').then(m => m.HomePage)
        .then(component => ({ default: component }))
      ),
  },
  CatchAllRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/pages/CatchAllPage').then(m => m.CatchAllPage)
        .then(component => ({ default: component }))
      ),
  },
  SignupRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/auth/pages/Signup').then(m => m.default)
        .then(component => ({ default: component }))
      ),
  },
  LoginRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/auth/pages/Login').then(m => m.default)
        .then(component => ({ default: component }))
      ),
  },
  PasswordResetRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/auth/pages/PasswordReset').then(m => m.PasswordReset)
        .then(component => ({ default: component }))
      ),
  },
  EmailVerificationRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/auth/pages/EmailVerification').then(m => m.EmailVerification)
        .then(component => ({ default: component }))
      ),
  },
  RequestPasswordResetRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/auth/pages/RequestPasswordReset').then(m => m.RequestPasswordReset)
        .then(component => ({ default: component }))
      ),
  },
  ProfileRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/auth/pages/ProfilePage').then(m => m.ProfilePage)
        .then(component => createAuthRequiredPage(component))
        .then(component => ({ default: component }))
      ),
  },
  ManualSignupRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/auth/pages/ManualSignupPage').then(m => m.ManualSignupPage)
        .then(component => ({ default: component }))
      ),
  },
  CustomSignupRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/auth/pages/CustomSignupPage').then(m => m.CustomSignupPage)
        .then(component => ({ default: component }))
      ),
  },
  TasksRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/operations/pages/TasksPage').then(m => m.TasksPage)
        .then(component => createAuthRequiredPage(component))
        .then(component => ({ default: component }))
      ),
  },
  TaskRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/operations/pages/TaskDetailPage').then(m => m.TaskDetailPage)
        .then(component => createAuthRequiredPage(component))
        .then(component => ({ default: component }))
      ),
  },
  SerializationRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/operations/pages/SerializationPage').then(m => m.SerializationPage)
        .then(component => ({ default: component }))
      ),
  },
  JobsRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/jobs/pages/JobsPage').then(m => m.JobsPage)
        .then(component => createAuthRequiredPage(component))
        .then(component => ({ default: component }))
      ),
  },
  ApisRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/apis/pages/ApisPage').then(m => m.ApisPage)
        .then(component => ({ default: component }))
      ),
  },
  CrudListRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/crud/pages/ListPage').then(m => m.ListPage)
        .then(component => createAuthRequiredPage(component))
        .then(component => ({ default: component }))
      ),
  },
  CrudDetailRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/crud/pages/DetailPage').then(m => m.DetailPage)
        .then(component => createAuthRequiredPage(component))
        .then(component => ({ default: component }))
      ),
  },
  StreamingRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/streaming/pages/StreamingTestPage').then(m => m.StreamingTestPage)
        .then(component => ({ default: component }))
      ),
  },
  ChatRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
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
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/lazy-loading/pages/LazyPage').then(m => m.LazyPage)
        .then(component => ({ default: component }))
      ),
  },
  PrerenderRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/prerender/pages/PrerenderPage').then(m => m.PrerenderPage)
        .then(component => ({ default: component }))
      ),
  },
  HydrationMismatchRoute: {
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      // https://github.com/remix-run/react-router/issues/14955
      lazy(() =>
        import('./src/features/prerender/pages/HydrationMismatchPage').then(m => m.HydrationMismatchPage)
        .then(component => ({ default: component }))
      ),
  },
} as const;

await setup_ext()

initializeQueryClient()

const rootElement =
  <App_ext />

export const routeObjects = getRouteObjects({
  routesMapping,
  rootElement,
})
