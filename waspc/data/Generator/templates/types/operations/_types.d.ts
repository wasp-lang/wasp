{{={= =}=}}
{=# operations =}
{=& jsFn.importStatement =}
{=/ operations =}

declare module 'wasp/types' {
  interface Register {
{=# operations =}
    'operation_{= operationName =}': typeof {= jsFn.importIdentifier =}
{=/ operations =}
  }
}
