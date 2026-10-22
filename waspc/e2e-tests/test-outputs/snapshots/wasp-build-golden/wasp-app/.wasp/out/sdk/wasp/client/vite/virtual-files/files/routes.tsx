import { getRouteObjects } from "wasp/client/app/router";
// Virtual files are part of the user's app, which depends on `wasp` but not
// on Wasp's internal libraries, so we import through `wasp/...`.
import { initializeQueryClient } from "wasp/client/operations";
import { Outlet } from "react-router"





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
