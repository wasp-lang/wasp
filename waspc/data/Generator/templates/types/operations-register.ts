{{={= =}=}}
// The import ensures the module is always loaded into the bundle.
// Otherwise, the module augmentation can fail if it wasn't loaded.
import "wasp/types"

declare module 'wasp/types' {
  interface OperationsRegister {
{=# operations =}
    '{= operationName =}': typeof {=& jsFn.dynamicImportExpression =}
{=/ operations =}
  }
}
