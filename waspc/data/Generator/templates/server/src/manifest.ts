{{={= =}=}}
{=# prismaSetupFn.isDefined =}
{=& prismaSetupFn.importStatement =}
import { registerPrismaSetupFn } from 'wasp/server/dbRegistry'

registerPrismaSetupFn({= prismaSetupFn.importIdentifier =})
{=/ prismaSetupFn.isDefined =}
