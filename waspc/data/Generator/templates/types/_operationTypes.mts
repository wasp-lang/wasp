{{={= =}=}}
declare module 'wasp/types' {
  interface OperationsRegistry {
{=# operations =}
    '{= operationName =}': {=& jsFn.typeofImportExpression =}
{=/ operations =}
  }
}
