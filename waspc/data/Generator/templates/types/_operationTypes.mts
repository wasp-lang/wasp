{{={= =}=}}
declare module 'wasp/types' {
  interface OperationsRegistry {
{=# operations =}
    '{= operationName =}': typeof {=& jsFn.dynamicImportExpr =}
{=/ operations =}
  }
}
