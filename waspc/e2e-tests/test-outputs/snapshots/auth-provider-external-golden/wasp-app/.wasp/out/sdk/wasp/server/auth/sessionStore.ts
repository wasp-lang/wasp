import type { CredentialStore } from './handler/types.js'

/**
 * The `prisma` credential store: every Wasp-issued credential that uses it is
 * a row in the app's `Session` table, created and revoked
 * through this module.
 *
 * Lucia is the implementation detail behind it. Nothing outside this module may
 * touch Lucia for session work, so migrating off it is a one-module change.
 */

// PRIVATE API
export const prismaCredentialStore: CredentialStore = {
  // No scheme keeps credentials in the database, so the Session model does
  // not exist. Reaching this store is a generator bug.
  create: () => unavailable(),
  get: () => unavailable(),
  delete: () => unavailable(),
  deleteAllForAuthId: () => unavailable(),
}

function unavailable(): never {
  throw new Error("No auth scheme uses the 'prisma' credential store, so there is no Session table to use.")
}
