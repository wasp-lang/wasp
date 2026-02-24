// Registrations that must complete before manifest.js runs.
//
// manifest.js imports user operation code, which transitively imports SDK
// modules (e.g., wasp/server → wasp/server/dbClient.ts). Those SDK modules call
// registry getters (getPrismaClient) at module scope.
//
// By importing registrations.js before manifest.js in server.ts, we ensure
// these registrations execute first — before any user operation code triggers
// SDK evaluation. This avoids the ESM initialization ordering problem where
// getters would return empty/fallback values.
//
// Operations don't need this treatment because their getOperation() uses a
// lazy wrapper (see operationsRegistry.ts).
//
// Note: serverEnvSchema is wired via a Rollup virtual module
// (virtual:wasp/user-server-env), not via a registry.
{{={= =}=}}
{=# prismaSetupFn.isDefined =}
import { registerPrismaClient } from 'wasp/server/dbRegistry'
{=& prismaSetupFn.importStatement =}
registerPrismaClient({= prismaSetupFn.importIdentifier =}())
{=/ prismaSetupFn.isDefined =}
