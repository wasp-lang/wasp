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
  AdminRoute: {
    lazy: async () => {
      const Component = await import('./src/AdminPage').then(m => m.AdminPage);

      return {
        Component:
          createAuthRequiredPage(Component, { providers: ['wasp'] }),
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
