{{={= =}=}}
import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";
import { lazy } from "react"

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
    Component:
      // We use React's `lazy()` instead of defining a Lazy Route on React
      // Router's side because there's a bug where it will ask for a
      // HydrationFallback and commit it immediately even when working with
      // prerendered pages.
      lazy(() =>
        {=& import.dynamicImportExpression =}
        {=# isAuthRequired =}
        .then(component => createAuthRequiredPage(component))
        {=/ isAuthRequired =}
        .then(component => ({ default: component }))
      ),
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
  {=# rootComponent.isDefined =}
  <{= rootComponent.importIdentifier =} />
  {=/ rootComponent.isDefined =}
  {=^ rootComponent.isDefined =}
  undefined
  {=/ rootComponent.isDefined =}

export const routeObjects = getRouteObjects({
  routesMapping,
  rootElement,
})
