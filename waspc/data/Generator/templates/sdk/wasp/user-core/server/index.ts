import type { PrismaClient } from './dbClient'

// PUBLIC API
export { default as config } from './config'
// PUBLIC API
export { default as prisma, type PrismaClient } from './dbClient'
// PUBLIC API
export { type ServerSetupFn } from '../../core/server/types/index'
// PUBLIC API
export { HttpError } from '../../core/server/HttpError'
// PUBLIC API
export { MiddlewareConfigFn } from '../../core/server/middleware/index'
// PUBLIC API
export { env } from './env'

// PUBLIC API
export type DbSeedFn = (prisma: PrismaClient) => Promise<void>
