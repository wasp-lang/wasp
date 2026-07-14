import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";

import { createAuthRequiredPage } from "wasp/client/app"

import { App as App_ext } from './src/App'

import { clientSetup as clientSetup_ext } from './src/clientSetup'

import { EagerPage } from './src/features/lazy-loading/pages/EagerPage'

const routesMapping = {
  HomeRoute: {
    lazy: async () => {
      const Component = await import('./src/pages/HomePage').then(m => m.HomePage);

      return {
        Component:
          Component,
      }
    },
  },
  FullStackModuleApiRoute: {
    lazy: async () => {
      const Component = await import('./src/pages/FullStackModuleApiPage').then(m => m.FullStackModuleApiPage);

      return {
        Component:
          Component,
      }
    },
  },
  CatchAllRoute: {
    lazy: async () => {
      const Component = await import('./src/pages/CatchAllPage').then(m => m.CatchAllPage);

      return {
        Component:
          Component,
      }
    },
  },
  SignupRoute: {
    lazy: async () => {
      const Component = await import('./src/features/auth/pages/Signup').then(m => m.default);

      return {
        Component:
          Component,
      }
    },
  },
  LoginRoute: {
    lazy: async () => {
      const Component = await import('./src/features/auth/pages/Login').then(m => m.default);

      return {
        Component:
          Component,
      }
    },
  },
  PasswordResetRoute: {
    lazy: async () => {
      const Component = await import('./src/features/auth/pages/PasswordReset').then(m => m.PasswordReset);

      return {
        Component:
          Component,
      }
    },
  },
  EmailVerificationRoute: {
    lazy: async () => {
      const Component = await import('./src/features/auth/pages/EmailVerification').then(m => m.EmailVerification);

      return {
        Component:
          Component,
      }
    },
  },
  RequestPasswordResetRoute: {
    lazy: async () => {
      const Component = await import('./src/features/auth/pages/RequestPasswordReset').then(m => m.RequestPasswordReset);

      return {
        Component:
          Component,
      }
    },
  },
  ProfileRoute: {
    lazy: async () => {
      const Component = await import('./src/features/auth/pages/ProfilePage').then(m => m.ProfilePage);

      return {
        Component:
          createAuthRequiredPage(Component),
      }
    },
  },
  ManualSignupRoute: {
    lazy: async () => {
      const Component = await import('./src/features/auth/pages/ManualSignupPage').then(m => m.ManualSignupPage);

      return {
        Component:
          Component,
      }
    },
  },
  CustomSignupRoute: {
    lazy: async () => {
      const Component = await import('./src/features/auth/pages/CustomSignupPage').then(m => m.CustomSignupPage);

      return {
        Component:
          Component,
      }
    },
  },
  TasksRoute: {
    lazy: async () => {
      const Component = await import('./src/features/operations/pages/TasksPage').then(m => m.TasksPage);

      return {
        Component:
          createAuthRequiredPage(Component),
      }
    },
  },
  TaskRoute: {
    lazy: async () => {
      const Component = await import('./src/features/operations/pages/TaskDetailPage').then(m => m.TaskDetailPage);

      return {
        Component:
          createAuthRequiredPage(Component),
      }
    },
  },
  SerializationRoute: {
    lazy: async () => {
      const Component = await import('./src/features/operations/pages/SerializationPage').then(m => m.SerializationPage);

      return {
        Component:
          Component,
      }
    },
  },
  JobsRoute: {
    lazy: async () => {
      const Component = await import('./src/features/jobs/pages/JobsPage').then(m => m.JobsPage);

      return {
        Component:
          createAuthRequiredPage(Component),
      }
    },
  },
  ApisRoute: {
    lazy: async () => {
      const Component = await import('./src/features/apis/pages/ApisPage').then(m => m.ApisPage);

      return {
        Component:
          Component,
      }
    },
  },
  CrudListRoute: {
    lazy: async () => {
      const Component = await import('./src/features/crud/pages/ListPage').then(m => m.ListPage);

      return {
        Component:
          createAuthRequiredPage(Component),
      }
    },
  },
  CrudDetailRoute: {
    lazy: async () => {
      const Component = await import('./src/features/crud/pages/DetailPage').then(m => m.DetailPage);

      return {
        Component:
          createAuthRequiredPage(Component),
      }
    },
  },
  StreamingRoute: {
    lazy: async () => {
      const Component = await import('./src/features/streaming/pages/StreamingTestPage').then(m => m.StreamingTestPage);

      return {
        Component:
          Component,
      }
    },
  },
  ChatRoute: {
    lazy: async () => {
      const Component = await import('./src/features/chat/pages/ChatPage').then(m => m.ChatPage);

      return {
        Component:
          createAuthRequiredPage(Component),
      }
    },
  },
  EagerRoute: {
    Component: EagerPage,
  },
  LazyRoute: {
    lazy: async () => {
      const Component = await import('./src/features/lazy-loading/pages/LazyPage').then(m => m.LazyPage);

      return {
        Component:
          Component,
      }
    },
  },
  PrerenderRoute: {
    lazy: async () => {
      const Component = await import('./src/features/prerender/pages/PrerenderPage').then(m => m.PrerenderPage);

      return {
        Component:
          Component,
      }
    },
  },
  PrerenderInstancesRoute: {
    lazy: async () => {
      const Component = await import('./src/features/prerender/pages/PrerenderInstancesPage').then(m => m.PrerenderInstancesPage);

      return {
        Component:
          Component,
      }
    },
  },
  HydrationMismatchRoute: {
    lazy: async () => {
      const Component = await import('./src/features/prerender/pages/HydrationMismatchPage').then(m => m.HydrationMismatchPage);

      return {
        Component:
          Component,
      }
    },
  },
  ModuleRoute: {
    lazy: async () => {
      const Component = await import('@kitchen-sink/module/MainPage').then(m => m.MainPage);

      return {
        Component:
          createAuthRequiredPage(Component),
      }
    },
  },
} as const;

await clientSetup_ext()

initializeQueryClient()

const rootElement =
  // We don't really need to wrap the app in a div nor name it "root", but we
  // keep it for backwards compatibility with older Wasp versions.
  <div id="root">
    <App_ext />
  </div>

export const routeObjects = getRouteObjects({
  routesMapping,
  rootElement,
})
