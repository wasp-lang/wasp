// @ts-nocheck
import { createAuthRequiredPage } from "wasp/client/app"

export const routesMapping = {
  HomeRoute: async () => {
    const { HomePage: Component } = await import('./src/pages/HomePage')
    return { Component }
  },
  CatchAllRoute: async () => {
    const { CatchAllPage: Component } = await import('./src/pages/CatchAllPage')
    return { Component }
  },
  SignupRoute: async () => {
    const { default: Component } = await import('./src/features/auth/pages/Signup')
    return { Component }
  },
  LoginRoute: async () => {
    const { default: Component } = await import('./src/features/auth/pages/Login')
    return { Component }
  },
  PasswordResetRoute: async () => {
    const { PasswordReset: Component } = await import('./src/features/auth/pages/PasswordReset')
    return { Component }
  },
  EmailVerificationRoute: async () => {
    const { EmailVerification: Component } = await import('./src/features/auth/pages/EmailVerification')
    return { Component }
  },
  RequestPasswordResetRoute: async () => {
    const { RequestPasswordReset: Component } = await import('./src/features/auth/pages/RequestPasswordReset')
    return { Component }
  },
  ProfileRoute: async () => {
    const { ProfilePage: Component } = await import('./src/features/auth/pages/ProfilePage')
    return { Component: createAuthRequiredPage(Component) }
  },
  ManualSignupRoute: async () => {
    const { ManualSignupPage: Component } = await import('./src/features/auth/pages/ManualSignupPage')
    return { Component }
  },
  CustomSignupRoute: async () => {
    const { CustomSignupPage: Component } = await import('./src/features/auth/pages/CustomSignupPage')
    return { Component }
  },
  TasksRoute: async () => {
    const { TasksPage: Component } = await import('./src/features/operations/pages/TasksPage')
    return { Component: createAuthRequiredPage(Component) }
  },
  TaskRoute: async () => {
    const { TaskDetailPage: Component } = await import('./src/features/operations/pages/TaskDetailPage')
    return { Component: createAuthRequiredPage(Component) }
  },
  SerializationRoute: async () => {
    const { SerializationPage: Component } = await import('./src/features/operations/pages/SerializationPage')
    return { Component }
  },
  JobsRoute: async () => {
    const { JobsPage: Component } = await import('./src/features/jobs/pages/JobsPage')
    return { Component: createAuthRequiredPage(Component) }
  },
  ApisRoute: async () => {
    const { ApisPage: Component } = await import('./src/features/apis/pages/ApisPage')
    return { Component }
  },
  CrudListRoute: async () => {
    const { ListPage: Component } = await import('./src/features/crud/pages/ListPage')
    return { Component: createAuthRequiredPage(Component) }
  },
  CrudDetailRoute: async () => {
    const { DetailPage: Component } = await import('./src/features/crud/pages/DetailPage')
    return { Component: createAuthRequiredPage(Component) }
  },
  StreamingRoute: async () => {
    const { StreamingTestPage: Component } = await import('./src/features/streaming/pages/StreamingTestPage')
    return { Component }
  },
  ChatRoute: async () => {
    const { ChatPage: Component } = await import('./src/features/chat/pages/ChatPage')
    return { Component: createAuthRequiredPage(Component) }
  },
} as const;
