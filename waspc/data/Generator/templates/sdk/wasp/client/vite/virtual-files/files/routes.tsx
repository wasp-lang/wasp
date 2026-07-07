{{={= =}=}}
import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";
{=^ rootComponent.isDefined =}
import { Outlet } from "react-router"
{=/ rootComponent.isDefined =}

{=# isAuthEnabled =}
import { createAuthRequiredPage } from "wasp/client/app"
{=/ isAuthEnabled =}

{=# rootComponent.isDefined =}
{=& rootComponent.importStatement =}
{=/ rootComponent.isDefined =}

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
{=/ setupFn.isDefined =}

{=# routes =}
{=^ isLazy =}
{=& import.importStatement =}
{=/ isLazy =}
{=/ routes =}

const routesMapping = {
  {=# routes =}
  {=# isLazy =}
  {= name =}: {
    lazy: async () => {
      const Component = await {=& import.dynamicImportExpression =};

      return {
        Component:
          {=# isAuthRequired =}
          createAuthRequiredPage(Component),
          {=/ isAuthRequired =}
          {=^ isAuthRequired =}
          Component,
          {=/ isAuthRequired =}
      }
    },
  },
  {=/ isLazy =}
  {=^ isLazy =}
  {= name =}: {
    {=# isAuthRequired =}
    Component: createAuthRequiredPage({= import.importIdentifier =}),
    {=/ isAuthRequired =}
    {=^ isAuthRequired =}
    Component: {= import.importIdentifier =},
    {=/ isAuthRequired =}
  },
  {=/ isLazy =}
  {=/ routes =}
} as const;

{=# setupFn.isDefined =}
await {= setupFn.importIdentifier =}()
{=/ setupFn.isDefined =}

initializeQueryClient()

const rootElement =
  // We don't really need to wrap the app in a div nor name it "root", but we
  // keep it for backwards compatibility with older Wasp versions.
  <div id="root">
    {=# rootComponent.isDefined =}
    <{= rootComponent.importIdentifier =} />
    {=/ rootComponent.isDefined =}
    {=^ rootComponent.isDefined =}
    <Outlet />
    {=/ rootComponent.isDefined =}
  </div>

export const routeObjects = getRouteObjects({
  routesMapping,
  rootElement,
})
