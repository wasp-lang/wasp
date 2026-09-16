import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";
import { Outlet } from "react-router"

import { createAuthRequiredPage } from "wasp/client/app"




const routesMapping = {
  MainRoute: {
    lazy: async () => {
      const Component = await import('./src/MainPage').then(m => m.MainPage);

      return {
        Component:
          createAuthRequiredPage(Component),
      }
    },
  },
  LoginRoute: {
    lazy: async () => {
      const Component = await import('./src/auth/LoginPage').then(m => m.LoginPage);

      return {
        Component:
          Component,
      }
    },
  },
  OAuthCallbackRoute: {
    lazy: async () => {
      const Component = await import('./src/auth/OAuthCallback').then(m => m.OAuthCallback);

      return {
        Component:
          Component,
      }
    },
  },
  EmailVerifiedRoute: {
    lazy: async () => {
      const Component = await import('./src/auth/EmailVerified').then(m => m.EmailVerified);

      return {
        Component:
          Component,
      }
    },
  },
  PasswordResetRoute: {
    lazy: async () => {
      const Component = await import('./src/auth/PasswordReset').then(m => m.PasswordReset);

      return {
        Component:
          Component,
      }
    },
  },
} as const;


initializeQueryClient()

const rootElement =
  // We don't really need to wrap the app in a div nor name it "root", but we
  // keep it for backwards compatibility with older Wasp versions.
  <div id="root">
    <Outlet />
  </div>

export const routeObjects = getRouteObjects({
  routesMapping,
  rootElement,
})
