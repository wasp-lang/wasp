import { join as joinPaths } from 'path'
import type { PrismaClient } from '@prisma/client'

// PUBLIC API
export { default as config } from './config.js'
// PUBLIC API
export { default as prisma } from './dbClient.js'
// PUBLIC API
export { type ServerSetupFn } from './types/index.js'
// PUBLIC API
export { HttpError } from './HttpError.js'
// PUBLIC API
export { MiddlewareConfigFn } from './middleware/index.js'

// PUBLIC API
export type DbSeedFn = (prisma: PrismaClient) => Promise<void>


// PUBLIC API
/**
 * Wasp runs the client code in the `web-app` directory which is nested in the
 * .wasp/out/web-app directory. This function resolves a project root dir path
 * to be relative to the `web-app` directory i.e. `../../../projectDirPath`.
 */
export function resolveProjectPath(path: string): string {
  return joinPaths('../../../', path)
}
