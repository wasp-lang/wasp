import type { ResolvedPrismaClient } from './dbClient.js';
export { default as config } from './config.js';
export { default as prisma, type ResolvedPrismaClient } from './dbClient.js';
export { type ServerSetupFn } from './types/index.js';
export { HttpError } from './HttpError.js';
export { MiddlewareConfigFn } from './middleware/index.js';
export { env } from './env.js';
export type DbSeedFn = (prisma: ResolvedPrismaClient) => Promise<void>;
//# sourceMappingURL=index.d.ts.map