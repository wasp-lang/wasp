import type { PrismaClient } from './dbClient.js'

// PUBLIC API
export { default as config } from './config.js'
// PUBLIC API
export { default as prisma, type PrismaClient } from './dbClient.js'
// PUBLIC API
export { type ServerSetupFn } from './types/index.js'
// PUBLIC API
export { HttpError } from './HttpError.js'
// PUBLIC API
export { MiddlewareConfigFn } from './middleware/index.js'
// PUBLIC API
export { env } from './env.js'

// PUBLIC API
export type DbSeedFn = (prisma: PrismaClient) => Promise<void>

// Ensure TypeScript includes server/module/index.ts in the compilation.
// Without this, wasp/server/module is only referenced by external module
// packages (.d.ts files in node_modules), which resolves to dist/ and
// causes TS5055 on rebuild.
export type { OperationContext as _OperationContext } from 'wasp/server/module'
