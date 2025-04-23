import type { PrismaClient } from '@prisma/client'
import type { ResolvedPrismaClient } from './dbClient.js'


// PUBLIC API
export { default as config } from './config.js'
// PUBLIC API
export { default as prisma, type ResolvedPrismaClient } from './dbClient.js'
// PUBLIC API
export { type ServerSetupFn } from './types/index.js'
// PUBLIC API
export { HttpError } from './HttpError.js'
// PUBLIC API
export { MiddlewareConfigFn } from './middleware/index.js'
// PUBLIC API
export { env } from './env.js'

// PUBLIC API

// PUBLIC API
export type DbSeedFn = (prisma: ResolvedPrismaClient) => Promise<void>

// PUBLIC API
// TODO: We can't use this type becuase prisma.$extends() doesn't return a PrismaClient?
export type PrismaSetupFn = () => PrismaClient
