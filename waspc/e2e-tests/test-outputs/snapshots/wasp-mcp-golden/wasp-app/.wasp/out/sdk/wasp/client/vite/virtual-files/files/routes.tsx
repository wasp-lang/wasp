import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";
import { Outlet } from "react-router"

import { createAuthRequiredPage } from "wasp/client/app"




const routesMapping = {
  RootRoute: {
    lazy: async () => {
      const Component = await import("./src/MainPage").then(m => m.MainPage);

      return {
        Component:
          Component,
      }
    },
  },
  TaskRoute: {
    lazy: async () => {
      const Component = await import("./src/MainPage").then(m => m.MainPage);

      return {
        Component:
          Component,
      }
    },
  },
  LoginRoute: {
    lazy: async () => {
      const Component = await import("./src/auth/AuthPages").then(m => m.LoginPage);

      return {
        Component:
          Component,
      }
    },
  },
  SignupRoute: {
    lazy: async () => {
      const Component = await import("./src/auth/AuthPages").then(m => m.SignupPage);

      return {
        Component:
          Component,
      }
    },
  },
  McpConsentRoute: {
    lazy: async () => {
      const Component = await import("./src/mcp/ConnectPage").then(m => m.ConnectPage);

      return {
        Component:
          createAuthRequiredPage(Component),
      }
    },
  },
  McpConfirmRoute: {
    lazy: async () => {
      const Component = await import("./src/mcp/ConfirmPage").then(m => m.ConfirmPage);

      return {
        Component:
          createAuthRequiredPage(Component),
      }
    },
  },
  ConnectionsRoute: {
    lazy: async () => {
      const Component = await import("./src/mcp/ConnectionsPage").then(m => m.ConnectionsPage);

      return {
        Component:
          createAuthRequiredPage(Component),
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
