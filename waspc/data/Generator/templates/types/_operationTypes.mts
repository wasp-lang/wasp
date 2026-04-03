{{={= =}=}}
declare module 'wasp/types' {
  interface OperationsRegistry {
{=# operations =}
    '{= operationName =}': typeof {=& jsFn.typeDynamicImportExpression =}
{=/ operations =}
  }
}
