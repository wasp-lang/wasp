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
  {= name =}: { lazy: async () => {
    const Component = await {=& import.runtimeDynamicImportExpression =}
    {=# isAuthRequired =}
    return { Component: createAuthRequiredPage(Component) }
    {=/ isAuthRequired =}
    {=^ isAuthRequired =}
    return { Component }
    {=/ isAuthRequired =}
  }},
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
