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
/**
 * The store of one scheme. All schemes share the `Session` table; the
 * `credentialScheme` column keeps their rows apart, so a row never
 * authenticates under another scheme and a per-scheme sign-out never sweeps
 * another scheme's rows.
 */
export function makePrismaCredentialStore(credentialScheme: string): CredentialStore {
  return {
  {=# isPrismaStoreUsed =}
  async create(record) {
    const session = await lucia.createSession(record.authId, { loginScheme: record.loginScheme, credentialScheme })
    // Lucia stamps its own default lifetime on the row. The record's is the
    // one that counts: the scheme's `ttl`, or a per-sign-in `ttl`.
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
    if (session === null || session.credentialScheme !== credentialScheme) {
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
  async deleteAllForAuthId(authId) {
    await prisma.{= sessionEntityLower =}.deleteMany({ where: { userId: authId, credentialScheme } })
  },
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
}
{=^ isPrismaStoreUsed =}

function unavailable(): never {
  throw new Error("No auth scheme uses the 'prisma' credential store, so there is no Session table to use.")
}
{=/ isPrismaStoreUsed =}
