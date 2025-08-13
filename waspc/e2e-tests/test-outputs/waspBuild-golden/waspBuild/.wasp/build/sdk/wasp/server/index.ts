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
