{{={= =}=}}
// @ts-nocheck
{=# isAuthEnabled =}
import { createAuthRequiredPage } from "wasp/client/app"
{=/ isAuthEnabled =}
{=# eagerImports =}
{=& importStatement =}
{=/ eagerImports =}

export const routesMapping = {
  {=# routes =}
  {=# isLazy =}
  {= name =}: { lazy: async () => {
    {=# isDefaultExport =}
    const { default: Component } = await import('{=& importPath =}')
    {=/ isDefaultExport =}
    {=^ isDefaultExport =}
    const { {= importIdentifier =}: Component } = await import('{=& importPath =}')
    {=/ isDefaultExport =}
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
    Component: createAuthRequiredPage({= importIdentifier =}),
    {=/ isAuthRequired =}
    {=^ isAuthRequired =}
    Component: {= importIdentifier =},
    {=/ isAuthRequired =}
  },
  {=/ isLazy =}
  {=/ routes =}
} as const;
