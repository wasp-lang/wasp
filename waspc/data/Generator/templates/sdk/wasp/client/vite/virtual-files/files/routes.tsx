{{={= =}=}}
// @ts-nocheck
import { getRouteObjects } from "wasp/client/app/router";
import { initializeQueryClient } from "wasp/client/operations";

{=# isAuthEnabled =}
import { createAuthRequiredPage } from "wasp/client/app"
{=/ isAuthEnabled =}

// These files are used from user-land and the import paths below are relative to the
// user's project dir, and not the SDK:
{=# pagesToImport =}
{=& importStatement =}
{=/ pagesToImport =}

const routesMapping = {
  {=# routes =}
  {= name =}: {= targetComponent =},
  {=/ routes =}
} as const;

{=# rootComponent.isDefined =}
{=& rootComponent.importStatement =}
{=/ rootComponent.isDefined =}

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
{=/ setupFn.isDefined =}

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
