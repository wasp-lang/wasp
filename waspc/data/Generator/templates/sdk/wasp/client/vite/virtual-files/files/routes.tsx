{{={= =}=}}
import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";

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
    {=! Lazy Route Modules are only safe during hydration because the client =}
    {=! entry waits for the router to be initialized (i.e. for the matched =}
    {=! route's `lazy` module to be loaded) before rendering `RouterProvider`. =}
    {=! See `client-entry.tsx` for details. =}
    lazy: () =>
      {=& import.dynamicImportExpression =}
      {=# isAuthRequired =}
      .then(component => createAuthRequiredPage(component))
      {=/ isAuthRequired =}
      .then(component => ({ Component: component })),
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
