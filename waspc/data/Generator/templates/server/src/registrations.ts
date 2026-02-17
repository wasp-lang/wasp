// Registrations that must complete before manifest.js runs.
//
// manifest.js imports user operation code, which transitively imports SDK
// modules (e.g., wasp/server → wasp/server/env.ts). Those SDK modules call
// registry getters (getServerEnvSchema, getPrismaSetupFn) at module scope.
//
// By importing registrations.js before manifest.js in server.ts, we ensure
// these registrations execute first — before any user operation code triggers
// SDK evaluation. This avoids the ESM initialization ordering problem where
// getters would return empty/fallback values.
//
// Operations don't need this treatment because their getOperation() uses a
// lazy wrapper (see operationsRegistry.ts).
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
