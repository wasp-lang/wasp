{{={= =}=}}
import { createHash, randomBytes } from 'node:crypto'
import type { AccountPrincipal } from './handler/types.js'
import { prisma } from '../index.js'

/**
 * One-time codes: a short-lived, single-use stand-in for an ACCOUNT, for a
 * browser navigation that cannot carry a bearer credential. Rows in Wasp's
 * own `{= oneTimeCodeEntityUpper =}` table, so they work for every scheme
 * whoever owns the credential, and need no secret. The code is stored hashed;
 * spending it is one guarded update, so two concurrent redemptions are
 * settled by the database whichever server instance they hit.
 */

const ONE_TIME_CODE_LIFETIME_MS = 60_000

// PRIVATE API
export async function createOneTimeCode(account: { authId: string; loginScheme: string }): Promise<string> {
  await deleteStaleCodes()
  const oneTimeCode = randomBytes(32).toString('base64url')
  await prisma.{= oneTimeCodeEntityLower =}.create({
    data: {
      code: hashCode(oneTimeCode),
      authId: account.authId,
      loginScheme: account.loginScheme,
      expiresAt: new Date(Date.now() + ONE_TIME_CODE_LIFETIME_MS),
    },
  })
  return oneTimeCode
}

// PRIVATE API
/** Who a one-time code stands for. Spends it: a second redemption is null. */
export async function redeemOneTimeCode(
  oneTimeCode: string,
): Promise<(AccountPrincipal & { loginScheme: string }) | null> {
  const { count } = await prisma.{= oneTimeCodeEntityLower =}.updateMany({
    where: { code: hashCode(oneTimeCode), usedAt: null, expiresAt: { gt: new Date() } },
    data: { usedAt: new Date() },
  })
  if (count === 0) {
    return null
  }
  const record = await prisma.{= oneTimeCodeEntityLower =}.findUnique({ where: { code: hashCode(oneTimeCode) } })
  if (record === null) {
    return null
  }
  // The code stood for an account, not for a login moment: it says who,
  // not how recently they logged in.
  return {
    authId: record.authId,
    loginScheme: record.loginScheme,
    credentialIssuedAt: null,
    isCredentialFresh: false,
  }
}

function hashCode(oneTimeCode: string): string {
  return createHash('sha256').update(oneTimeCode).digest('hex')
}

async function deleteStaleCodes(): Promise<void> {
  await prisma.{= oneTimeCodeEntityLower =}.deleteMany({
    where: { expiresAt: { lt: new Date(Date.now() - 10 * ONE_TIME_CODE_LIFETIME_MS) } },
  })
}
