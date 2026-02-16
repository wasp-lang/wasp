{{={= =}=}}
{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}
import { registerPrismaSetupFn } from 'wasp/server/dbRegistry'

registerPrismaSetupFn({= prismaSetupFn.importIdentifier =})
{=/ prismaSetupFn.isDefined =}

{=# hasOperations =}
import { registerOperation } from 'wasp/server/operations/operationsRegistry'
{=/ hasOperations =}
{=# operations =}
{=& jsFn.importStatement =}
{=/ operations =}
{=# operations =}
registerOperation('{= operationName =}', {= jsFn.importIdentifier =})
{=/ operations =}

{=# serverEnvValidationSchema.isDefined =}
{=& serverEnvValidationSchema.importStatement =}
import { registerServerEnvSchema } from 'wasp/server/envRegistry'
registerServerEnvSchema({= serverEnvValidationSchema.importIdentifier =})
{=/ serverEnvValidationSchema.isDefined =}
