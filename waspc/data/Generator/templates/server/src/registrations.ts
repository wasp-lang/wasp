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
import { registerPrismaSetupFn } from 'wasp/server/dbRegistry'
{=& prismaSetupFn.importStatement =}
registerPrismaSetupFn({= prismaSetupFn.importIdentifier =})
{=/ prismaSetupFn.isDefined =}

import { registerServerEnvSchema } from 'wasp/server/envRegistry'
{=# serverEnvValidationSchema.isDefined =}
{=& serverEnvValidationSchema.importStatement =}
registerServerEnvSchema({= serverEnvValidationSchema.importIdentifier =})
{=/ serverEnvValidationSchema.isDefined =}
{=^ serverEnvValidationSchema.isDefined =}
import * as z from 'zod'
registerServerEnvSchema(z.object({}))
{=/ serverEnvValidationSchema.isDefined =}
