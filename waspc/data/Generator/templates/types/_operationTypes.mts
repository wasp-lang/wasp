{{={= =}=}}
declare module 'wasp/types' {
  interface OperationsRegistry {
{=# operations =}
    '{= operationName =}': typeof {=& jsFn.typeofImportExpression =}
{=/ operations =}
  }
}
