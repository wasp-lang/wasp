{{={= =}=}}
// @ts-nocheck
{=# isAuthEnabled =}
import { createAuthRequiredPage } from "wasp/client/app"
{=/ isAuthEnabled =}

// This file is used from user-land and the import paths below are relative to the
// user's project dir, and not the SDK:
{=# pagesToImport =}
{=& importStatement =}
{=/ pagesToImport =}

export const routesMapping = {
  {=# routes =}
  {= name =}: {= targetComponent =},
  {=/ routes =}
} as const;
