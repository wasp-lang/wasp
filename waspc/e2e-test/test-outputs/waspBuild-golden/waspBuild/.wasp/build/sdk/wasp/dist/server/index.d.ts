import type { PrismaClient } from '@prisma/client';
export { default as config } from './config.js';
export { default as prisma } from './dbClient.js';
export { type ServerSetupFn } from './types/index.js';
export { HttpError } from './HttpError.js';
export { MiddlewareConfigFn } from './middleware/index.js';
export type DbSeedFn = (prisma: PrismaClient) => Promise<void>;
