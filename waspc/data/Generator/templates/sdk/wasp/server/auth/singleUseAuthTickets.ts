{{={= =}=}}
import { createHash, randomBytes } from 'node:crypto'
import type { AccountPrincipal } from './handler/types.js'
import { prisma } from '../index.js'

/**
 * Single-use auth tickets: a short-lived, single-use stand-in for an ACCOUNT, for a
 * browser navigation that cannot carry a bearer credential. Rows in Wasp's
 * own `{= singleUseAuthTicketEntityUpper =}` table, so they work for every scheme
 * whoever owns the credential, and need no secret. The code is stored hashed;
 * spending it is one guarded update, so two concurrent redemptions are
 * settled by the database whichever server instance they hit.
 */

const AUTH_TICKET_LIFETIME_MS = 60_000

// PRIVATE API
export async function createSingleUseAuthTicket(account: { authId: string; loginScheme: string }): Promise<string> {
  await deleteStaleTickets()
  const singleUseAuthTicket = randomBytes(32).toString('base64url')
  await prisma.{= singleUseAuthTicketEntityLower =}.create({
    data: {
      code: hashTicket(singleUseAuthTicket),
      authId: account.authId,
      loginScheme: account.loginScheme,
      expiresAt: new Date(Date.now() + AUTH_TICKET_LIFETIME_MS),
    },
  })
  return singleUseAuthTicket
}

// PRIVATE API
/** Who a single-use auth ticket stands for. Spends it: a second redemption is null. */
export async function redeemSingleUseAuthTicket(
  singleUseAuthTicket: string,
): Promise<(AccountPrincipal & { loginScheme: string }) | null> {
  const { count } = await prisma.{= singleUseAuthTicketEntityLower =}.updateMany({
    where: { code: hashTicket(singleUseAuthTicket), usedAt: null, expiresAt: { gt: new Date() } },
    data: { usedAt: new Date() },
  })
  if (count === 0) {
    return null
  }
  const record = await prisma.{= singleUseAuthTicketEntityLower =}.findUnique({ where: { code: hashTicket(singleUseAuthTicket) } })
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

function hashTicket(singleUseAuthTicket: string): string {
  return createHash('sha256').update(singleUseAuthTicket).digest('hex')
}

async function deleteStaleTickets(): Promise<void> {
  await prisma.{= singleUseAuthTicketEntityLower =}.deleteMany({
    where: { expiresAt: { lt: new Date(Date.now() - 10 * AUTH_TICKET_LIFETIME_MS) } },
  })
}
