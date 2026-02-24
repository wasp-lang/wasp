{{={= =}=}}
declare module 'wasp/types' {
  interface RegisteredOperations {
{=# operations =}
    '{= operationName =}': typeof {=& jsFn.typeofImportExpr =}
{=/ operations =}
  }
}
