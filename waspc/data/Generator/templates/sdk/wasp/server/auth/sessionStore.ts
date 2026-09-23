{{={= =}=}}
import type { CredentialStore } from './handler/types.js'
{=# isPrismaStoreUsed =}
import { prisma } from '../index.js'
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
    const session = await lucia.createSession(record.authId, { loginScheme: record.loginScheme })
    // Lucia stamps its own default lifetime on the row. The record's is the
    // one that counts: the scheme's `ttl`, a per-sign-in `ttl`, or the one
    // minute of a one-time code.
    await prisma.{= sessionEntityLower =}.update({
      where: { id: session.id },
      data: { expiresAt: record.expiresAt, issuedAt: record.issuedAt },
    })
    return { id: session.id }
  },
  async get(id) {
    // Read the row directly. Lucia's `validateSession` silently EXTENDS a
    // session that is near its expiry to Lucia's default lifetime, which would
    // turn a 15 minute credential into a 30 day one.
    const session = await prisma.{= sessionEntityLower =}.findUnique({ where: { id } })
    if (session === null) {
      return null
    }
    if (session.expiresAt <= new Date()) {
      await prisma.{= sessionEntityLower =}.deleteMany({ where: { id } })
      return null
    }
    return {
      authId: session.userId,
      loginScheme: session.loginScheme,
      issuedAt: session.issuedAt,
      expiresAt: session.expiresAt,
    }
  },
  delete: (id) => lucia.invalidateSession(id),
  deleteAllForAuthId: (authId) => lucia.invalidateUserSessions(authId),
  async extend(id, expiresAt) {
    await prisma.{= sessionEntityLower =}.updateMany({ where: { id }, data: { expiresAt } })
  },
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
