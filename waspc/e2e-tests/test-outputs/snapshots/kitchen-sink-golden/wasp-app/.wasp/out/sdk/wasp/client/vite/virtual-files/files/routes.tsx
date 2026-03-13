// @ts-nocheck
import { createAuthRequiredPage } from "wasp/client/app"

export const routesMapping = {
  HomeRoute: { lazy: async () => {
    const { HomePage: Component } = await import('./src/pages/HomePage')
    return { Component }
  }},
  CatchAllRoute: { lazy: async () => {
    const { CatchAllPage: Component } = await import('./src/pages/CatchAllPage')
    return { Component }
  }},
  SignupRoute: { lazy: async () => {
    const { default: Component } = await import('./src/features/auth/pages/Signup')
    return { Component }
  }},
  LoginRoute: { lazy: async () => {
    const { default: Component } = await import('./src/features/auth/pages/Login')
    return { Component }
  }},
  PasswordResetRoute: { lazy: async () => {
    const { PasswordReset: Component } = await import('./src/features/auth/pages/PasswordReset')
    return { Component }
  }},
  EmailVerificationRoute: { lazy: async () => {
    const { EmailVerification: Component } = await import('./src/features/auth/pages/EmailVerification')
    return { Component }
  }},
  RequestPasswordResetRoute: { lazy: async () => {
    const { RequestPasswordReset: Component } = await import('./src/features/auth/pages/RequestPasswordReset')
    return { Component }
  }},
  ProfileRoute: { lazy: async () => {
    const { ProfilePage: Component } = await import('./src/features/auth/pages/ProfilePage')
    return { Component: createAuthRequiredPage(Component) }
  }},
  ManualSignupRoute: { lazy: async () => {
    const { ManualSignupPage: Component } = await import('./src/features/auth/pages/ManualSignupPage')
    return { Component }
  }},
  CustomSignupRoute: { lazy: async () => {
    const { CustomSignupPage: Component } = await import('./src/features/auth/pages/CustomSignupPage')
    return { Component }
  }},
  TasksRoute: { lazy: async () => {
    const { TasksPage: Component } = await import('./src/features/operations/pages/TasksPage')
    return { Component: createAuthRequiredPage(Component) }
  }},
  TaskRoute: { lazy: async () => {
    const { TaskDetailPage: Component } = await import('./src/features/operations/pages/TaskDetailPage')
    return { Component: createAuthRequiredPage(Component) }
  }},
  SerializationRoute: { lazy: async () => {
    const { SerializationPage: Component } = await import('./src/features/operations/pages/SerializationPage')
    return { Component }
  }},
  JobsRoute: { lazy: async () => {
    const { JobsPage: Component } = await import('./src/features/jobs/pages/JobsPage')
    return { Component: createAuthRequiredPage(Component) }
  }},
  ApisRoute: { lazy: async () => {
    const { ApisPage: Component } = await import('./src/features/apis/pages/ApisPage')
    return { Component }
  }},
  CrudListRoute: { lazy: async () => {
    const { ListPage: Component } = await import('./src/features/crud/pages/ListPage')
    return { Component: createAuthRequiredPage(Component) }
  }},
  CrudDetailRoute: { lazy: async () => {
    const { DetailPage: Component } = await import('./src/features/crud/pages/DetailPage')
    return { Component: createAuthRequiredPage(Component) }
  }},
  StreamingRoute: { lazy: async () => {
    const { StreamingTestPage: Component } = await import('./src/features/streaming/pages/StreamingTestPage')
    return { Component }
  }},
  ChatRoute: { lazy: async () => {
    const { ChatPage: Component } = await import('./src/features/chat/pages/ChatPage')
    return { Component: createAuthRequiredPage(Component) }
  }},
} as const;
