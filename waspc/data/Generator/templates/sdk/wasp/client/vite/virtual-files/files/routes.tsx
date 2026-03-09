{{={= =}=}}
// @ts-nocheck
{=# isAuthEnabled =}
import { createAuthRequiredPage } from "wasp/client/app"
{=/ isAuthEnabled =}

export const routesMapping = {
  {=# routes =}
  {= name =}: async () => {
    {=# isDefaultExport =}
    const { default: Component } = await import('{=& importPath =}')
    {=/ isDefaultExport =}
    {=^ isDefaultExport =}
    const { {= exportedName =}: Component } = await import('{=& importPath =}')
    {=/ isDefaultExport =}
    {=# isAuthRequired =}
    return { Component: createAuthRequiredPage(Component) }
    {=/ isAuthRequired =}
    {=^ isAuthRequired =}
    return { Component }
    {=/ isAuthRequired =}
  },
  {=/ routes =}
} as const;
