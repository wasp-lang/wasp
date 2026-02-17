{{={= =}=}}
{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}
import { registerPrismaSetupFn } from 'wasp/server/dbRegistry'

registerPrismaSetupFn({= prismaSetupFn.importIdentifier =})
{=/ prismaSetupFn.isDefined =}

{=# serverEnvValidationSchema.isDefined =}
{=& serverEnvValidationSchema.importStatement =}
import { registerServerEnvSchema } from 'wasp/server/envRegistry'
registerServerEnvSchema({= serverEnvValidationSchema.importIdentifier =})
{=/ serverEnvValidationSchema.isDefined =}
