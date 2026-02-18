{{={= =}=}}
// Forces the file to be interpreted as a module.
// This is necessary for module augmentation to work.
export {}

declare module 'wasp/types' {
  interface RegisteredOperations {
{=# operations =}
    '{= operationName =}': typeof {=& jsFn.typeofImportExpr =}
{=/ operations =}
  }
}
