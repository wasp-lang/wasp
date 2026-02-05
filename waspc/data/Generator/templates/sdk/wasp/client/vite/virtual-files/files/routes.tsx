{{={= =}=}}
// @ts-nocheck
{=# isAuthEnabled =}
import { createAuthRequiredPage } from "wasp/client/app"
{=/ isAuthEnabled =}

// These files are used from user-land and the import paths below are relative to the
// user's project dir, and not the SDK:
{=# pagesToImport =}
{=& importStatement =}
{=/ pagesToImport =}

function getOptionalExport(mod, exportName) {
  return mod && typeof mod === "object" ? mod[exportName] : undefined;
}

export const routesMapping = {
  {=# routes =}
  {= name =}: {= targetComponent =},
  {=/ routes =}
} as const;

export const routeNameToSsr = {
  {=# routes =}
  {= name =}: {=# ssr =}true{=/ ssr =}{=^ ssr =}false{=/ ssr =},
  {=/ routes =}
} as const;

export const routeNameToHead = {
  {=# routes =}
  {= name =}: getOptionalExport({= pageModuleIdentifier =}, "head"),
  {=/ routes =}
} as const;
