import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";

import { createAuthRequiredPage } from "wasp/client/app"

import { App as App_ext } from './src/App'

import { setup as setup_ext } from './src/clientSetup'

import { EagerPage } from './src/features/lazy-loading/pages/EagerPage'

const routesMapping = {
  HomeRoute: { lazy: async () => {
    const Component = await import('./src/pages/HomePage').then(m => m.HomePage)
    return { Component }
  }},
  CatchAllRoute: { lazy: async () => {
    const Component = await import('./src/pages/CatchAllPage').then(m => m.CatchAllPage)
    return { Component }
  }},
  SignupRoute: { lazy: async () => {
    const Component = await import('./src/features/auth/pages/Signup').then(m => m.default)
    return { Component }
  }},
  LoginRoute: { lazy: async () => {
    const Component = await import('./src/features/auth/pages/Login').then(m => m.default)
    return { Component }
  }},
  PasswordResetRoute: { lazy: async () => {
    const Component = await import('./src/features/auth/pages/PasswordReset').then(m => m.PasswordReset)
    return { Component }
  }},
  EmailVerificationRoute: { lazy: async () => {
    const Component = await import('./src/features/auth/pages/EmailVerification').then(m => m.EmailVerification)
    return { Component }
  }},
  RequestPasswordResetRoute: { lazy: async () => {
    const Component = await import('./src/features/auth/pages/RequestPasswordReset').then(m => m.RequestPasswordReset)
    return { Component }
  }},
  ProfileRoute: { lazy: async () => {
    const Component = await import('./src/features/auth/pages/ProfilePage').then(m => m.ProfilePage)
    return { Component: createAuthRequiredPage(Component) }
  }},
  ManualSignupRoute: { lazy: async () => {
    const Component = await import('./src/features/auth/pages/ManualSignupPage').then(m => m.ManualSignupPage)
    return { Component }
  }},
  CustomSignupRoute: { lazy: async () => {
    const Component = await import('./src/features/auth/pages/CustomSignupPage').then(m => m.CustomSignupPage)
    return { Component }
  }},
  TasksRoute: { lazy: async () => {
    const Component = await import('./src/features/operations/pages/TasksPage').then(m => m.TasksPage)
    return { Component: createAuthRequiredPage(Component) }
  }},
  TaskRoute: { lazy: async () => {
    const Component = await import('./src/features/operations/pages/TaskDetailPage').then(m => m.TaskDetailPage)
    return { Component: createAuthRequiredPage(Component) }
  }},
  SerializationRoute: { lazy: async () => {
    const Component = await import('./src/features/operations/pages/SerializationPage').then(m => m.SerializationPage)
    return { Component }
  }},
  JobsRoute: { lazy: async () => {
    const Component = await import('./src/features/jobs/pages/JobsPage').then(m => m.JobsPage)
    return { Component: createAuthRequiredPage(Component) }
  }},
  ApisRoute: { lazy: async () => {
    const Component = await import('./src/features/apis/pages/ApisPage').then(m => m.ApisPage)
    return { Component }
  }},
  CrudListRoute: { lazy: async () => {
    const Component = await import('./src/features/crud/pages/ListPage').then(m => m.ListPage)
    return { Component: createAuthRequiredPage(Component) }
  }},
  CrudDetailRoute: { lazy: async () => {
    const Component = await import('./src/features/crud/pages/DetailPage').then(m => m.DetailPage)
    return { Component: createAuthRequiredPage(Component) }
  }},
  StreamingRoute: { lazy: async () => {
    const Component = await import('./src/features/streaming/pages/StreamingTestPage').then(m => m.StreamingTestPage)
    return { Component }
  }},
  ChatRoute: { lazy: async () => {
    const Component = await import('./src/features/chat/pages/ChatPage').then(m => m.ChatPage)
    return { Component: createAuthRequiredPage(Component) }
  }},
  EagerRoute: {
    Component: EagerPage,
  },
  LazyRoute: { lazy: async () => {
    const Component = await import('./src/features/lazy-loading/pages/LazyPage').then(m => m.LazyPage)
    return { Component }
  }},
} as const;

await setup_ext()

initializeQueryClient()

const rootElement =
  <App_ext />

export const routeObjects = getRouteObjects({
  routesMapping,
  rootElement,
})
