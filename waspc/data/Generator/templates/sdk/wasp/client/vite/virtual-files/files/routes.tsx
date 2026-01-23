{{={= =}=}}
// @ts-nocheck
{=# isAuthEnabled =}
import { createAuthRequiredPage } from "wasp/client/app"
{=/ isAuthEnabled =}

{=# pagesToImport =}
{=& importStatement =}
{=/ pagesToImport =}

export const routesMapping = {
  {=# routes =}
  {= name =}: {= targetComponent =},
  {=/ routes =}
} as const;
