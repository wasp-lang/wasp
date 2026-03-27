{{={= =}=}}
// @ts-nocheck
{=# isAuthEnabled =}
import { createAuthRequiredPage } from "wasp/client/app"
{=/ isAuthEnabled =}
{=# routes =}
{=^ isLazy =}
{=& import.importStatement =}
{=/ isLazy =}
{=/ routes =}

export const routesMapping = {
  {=# routes =}
  {=# isLazy =}
  {= name =}: { lazy: async () => {
    const Component = await {=& import.dynamicImportExpression =}
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
