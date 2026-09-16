{{={= =}=}}
import type { CredentialStore } from './handler/types.js'
{=# isPrismaStoreUsed =}
import { auth as lucia } from './lucia.js'
{=/ isPrismaStoreUsed =}

/**
 * The `prisma` credential store: every Wasp-issued credential that uses it is
 * a row in the app's `{= sessionEntityUpper =}` table, created and revoked
 * through this module.
 *
 * Lucia is the implementation detail behind it. Nothing outside this module may
 * touch Lucia for session work, so migrating off it is a one-module change.
 */

// PRIVATE API
export const prismaCredentialStore: CredentialStore = {
  {=# isPrismaStoreUsed =}
  async create(record) {
    const session = await lucia.createSession(record.authId, { signedInBy: record.signedInBy })
    return { id: session.id }
  },
  async get(id) {
    const { session } = await lucia.validateSession(id)
    if (!session) {
      return null
    }
    return {
      authId: session.userId,
      signedInBy: session.signedInBy,
      issuedAt: new Date(0),
      expiresAt: session.expiresAt,
    }
  },
  delete: (id) => lucia.invalidateSession(id),
  deleteAllForAuthId: (authId) => lucia.invalidateUserSessions(authId),
  {=/ isPrismaStoreUsed =}
  {=^ isPrismaStoreUsed =}
  // No scheme keeps credentials in the database, so the Session model does
  // not exist. Reaching this store is a generator bug.
  create: () => unavailable(),
  get: () => unavailable(),
  delete: () => unavailable(),
  deleteAllForAuthId: () => unavailable(),
  {=/ isPrismaStoreUsed =}
}
{=^ isPrismaStoreUsed =}

function unavailable(): never {
  throw new Error("No auth scheme uses the 'prisma' credential store, so there is no Session table to use.")
}
{=/ isPrismaStoreUsed =}
