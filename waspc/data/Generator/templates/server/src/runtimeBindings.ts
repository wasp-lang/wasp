{{={= =}=}}
import type { ServerRuntimeBindings } from 'wasp/server/runtime'

{=# serverEnvValidationSchema.isDefined =}
{=& serverEnvValidationSchema.importStatement =}
{=/ serverEnvValidationSchema.isDefined =}
{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}
{=/ prismaSetupFn.isDefined =}
export const serverRuntimeBindings = {
  serverEnvValidationSchema: {=# serverEnvValidationSchema.isDefined =}{= serverEnvValidationSchema.importIdentifier =}{=/ serverEnvValidationSchema.isDefined =}{=^ serverEnvValidationSchema.isDefined =}undefined{=/ serverEnvValidationSchema.isDefined =},
  prismaSetupFn: {=# prismaSetupFn.isDefined =}{= prismaSetupFn.importIdentifier =}{=/ prismaSetupFn.isDefined =}{=^ prismaSetupFn.isDefined =}undefined{=/ prismaSetupFn.isDefined =},
  operations: {
    {=# operations =}
    '{= operationName =}': () => {=& jsFn.dynamicImportExpression =},
    {=/ operations =}
  },
} satisfies ServerRuntimeBindings
