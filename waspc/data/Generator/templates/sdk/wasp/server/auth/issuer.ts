{{={= =}=}}
import { TimeSpan, createJWTHelpers } from '@wasp.sh/lib-auth/node'
import type {
  AccountPrincipal,
  CredentialHandler,
  AuthIdentityKey,
  CredentialRecord,
  CredentialStore,
  SignInProperties,
  SignInResult,
} from './handler/types.js'
import { prisma } from '../index.js'
import { prismaCredentialStore } from './sessionStore.js'
import { getIdentityStore } from './identityStore.js'

/**
 * The framework's credential issuer: what backs every
 * `credentials: { transport, store }` in `main.wasp.ts`, inline on a scheme
 * or standalone as `waspBearer()` / `waspCookie()`.
 *
 * Two independent axes. The transport decides where the credential travels
 * (an Authorization header the generated client attaches, or an HttpOnly
 * cookie the browser attaches) and therefore how "no" is said (401 vs a
 * redirect). The store decides what the credential contains: a key to a row
 * (`prisma`), or the signed record itself (`signed-token`), or whatever the
 * app's own store makes of it.
 */

// PRIVATE API
export type IssuerOptions = {
  scheme: string
  transport: 'bearer' | 'cookie'
  store: 'prisma' | 'signed-token' | CredentialStore
  /** Credential lifetime, e.g. "30d", "15m". */
  ttl: string
  /** How long after a login the credential counts as fresh, e.g. "15m". */
  freshFor: string
  /** Sliding renewal window, e.g. "7d"; null when a credential lives exactly its ttl. */
  slidingRenewal: string | null
  /** Signing secret; required by the signed-token store. */
  secret?: string
  /** Where a browser navigation with no credential is sent (cookie transport). */
  loginPath: string
}

const COOKIE_NAME = 'wasp_credential'

// PRIVATE API
/** What a Wasp issuer knows about a credential it issued: the account, plus who verified the login. */
export type IssuedCredential = AccountPrincipal & { credentialId: string; signedInBy: string }

// PRIVATE API
/**
 * Wasp's own issuer: an ordinary `CredentialHandler` with no login of its own, one
 * per `waspBearer()` / `waspCookie()` scheme and one, private, per scheme
 * with inline `credentials`. Its credential carries the account, so it
 * answers `authenticate` with the account and who verified the login.
 */
export function createIssuer(options: IssuerOptions): CredentialHandler {
  const store = resolveStore(options)
  const transport = options.transport === 'cookie' ? cookieTransport(options) : bearerTransport
  const schemeTtl = parseTimeSpan(options.ttl)
  const freshFor = parseTimeSpan(options.freshFor)
  const renewBelow = options.slidingRenewal === null ? null : parseTimeSpan(options.slidingRenewal)
  if (renewBelow !== null && store.extend === undefined) {
    throw new Error(
      `Auth scheme '${options.scheme}' is configured with sliding renewal, but its credential store cannot extend a credential.`,
    )
  }

  const readCredential = async (request: Request): Promise<IssuedCredential | null> => {
      const id = transport.read(request)
      if (id === null) {
        return null
      }
      const record = await store.get(id)
      if (record === null) {
        return null
      }
      // Sliding renewal: an active user keeps a live credential. `issuedAt`
      // stays, so a renewed credential is no fresher than it was.
      if (renewBelow !== null && record.expiresAt.getTime() - Date.now() < renewBelow.milliseconds()) {
        await store.extend!(id, new Date(Date.now() + schemeTtl.milliseconds()))
      }
      return {
        authId: record.authId,
        signedInBy: record.signedInBy,
        credentialId: id,
        credentialIssuedAt: record.issuedAt,
        isCredentialFresh: Date.now() - record.issuedAt.getTime() < freshFor.milliseconds(),
      }
  }
  // The facet that calls in has resolved the identity within the calling
  // scheme and guarded its provider name, so this lookup cannot cross scheme
  // boundaries; `handlerName` IS the calling scheme.
  const findIdentity = async (identity: AuthIdentityKey) => {
    const found = await getIdentityStore(identity.handlerName, identity.providerName).find(identity.providerUserId)
    if (found === null) {
      throw contractError('wasp-auth/identity-not-found', 'No identity to issue a credential for.')
    }
    return found
  }

  return {
    async authenticate(request) {
      const credential = await readCredential(request)
      return credential === null
        ? { status: 'unauthenticated' }
        : { status: 'authenticated', account: credential, signedInBy: credential.signedInBy }
    },

    async signIn(identity: AuthIdentityKey, properties?: SignInProperties): Promise<SignInResult> {
      const { authId } = await findIdentity(identity)
      // Per-sign-in properties win over the scheme's configuration.
      const ttl = properties?.ttl !== undefined ? parseTimeSpan(properties.ttl) : schemeTtl
      const persistent = properties?.persistent ?? true
      const issuedAt = new Date()
      const { id } = await store.create({
        authId,
        signedInBy: identity.handlerName,
        issuedAt,
        expiresAt: new Date(issuedAt.getTime() + ttl.milliseconds()),
      })
      return {
        response: transport.write(id, { maxAgeSeconds: ttl.seconds(), persistent }),
        credentialId: id,
      }
    },

    async signOut(request) {
      const id = transport.read(request)
      if (id !== null) {
        await store.delete(id)
      }
      return transport.clear()
    },

    async signOutEverywhere(identity: AuthIdentityKey) {
      const { authId } = await findIdentity(identity)
      await store.deleteAllForAuthId(authId)
    },

    challenge: async (request) => transport.challenge(request),
    forbid: async () => Response.json({ message: 'Forbidden' }, { status: 403 }),
  }
}

// PRIVATE API
/** Every credential of the person behind an Auth entity, whichever scheme issued them. */
export async function signOutEverywhere(options: IssuerOptions, authId: string): Promise<void> {
  await resolveStore(options).deleteAllForAuthId(authId)
}

// ---- transports -------------------------------------------------------------

type Transport = {
  read(request: Request): string | null
  write(id: string, lifetime: CredentialLifetime): Response
  clear(): Response
  challenge(request: Request): Response
}

/** How long the client should keep a freshly issued credential. */
type CredentialLifetime = { maxAgeSeconds: number; persistent: boolean }

const bearerTransport: Transport = {
  read: (request) => {
    const header = request.headers.get('authorization')
    const prefix = 'Bearer '
    return header !== null && header.startsWith(prefix) ? header.substring(prefix.length) : null
  },
  // The generated client stores the credential and attaches it to every request.
  // `persistent: false` tells it to keep the credential for the browser
  // session only.
  write: (id, { persistent }): Response =>
    Response.json(persistent ? { credential: id } : { credential: id, persistent: false }),
  clear: () => Response.json({ success: true }),
  challenge: () => Response.json({ message: 'Invalid credentials' }, { status: 401 }),
}

function cookieTransport(options: IssuerOptions): Transport {
  const attributes = `Path=/; HttpOnly; SameSite=Lax${isDevelopment() ? '' : '; Secure'}`
  return {
    read: (request) => {
      const cookieHeader = request.headers.get('cookie') ?? ''
      for (const part of cookieHeader.split(';')) {
        const [name, ...rest] = part.trim().split('=')
        if (name === COOKIE_NAME) {
          return decodeURIComponent(rest.join('='))
        }
      }
      return null
    },
    // Without Max-Age the browser drops the cookie when its session ends.
    write: (id, { maxAgeSeconds, persistent }) =>
      Response.json(
        { success: true },
        {
          headers: {
            'Set-Cookie': `${COOKIE_NAME}=${encodeURIComponent(id)}; ${attributes}${persistent ? `; Max-Age=${maxAgeSeconds}` : ''}`,
          },
        },
      ),
    clear: () => Response.json({ success: true }, { headers: { 'Set-Cookie': `${COOKIE_NAME}=; ${attributes}; Max-Age=0` } }),
    // A browser navigation gets the login page; an API call gets a 401.
    challenge: (request) =>
      (request.headers.get('accept') ?? '').includes('text/html')
        ? Response.redirect(options.loginPath, 302)
        : Response.json({ message: 'Invalid credentials' }, { status: 401 }),
  }
}

function isDevelopment(): boolean {
  return process.env.NODE_ENV !== 'production'
}

// ---- stores -----------------------------------------------------------------

function resolveStore(options: IssuerOptions): CredentialStore {
  if (typeof options.store === 'object') {
    return options.store
  }
  if (options.store === 'prisma') {
    return prismaCredentialStore
  }
  if (options.secret === undefined) {
    throw new Error(`The signed-token credential store of scheme '${options.scheme}' needs a signing secret.`)
  }
  return signedTokenStore(options.secret)
}

/**
 * The record travels inside a signed token. No table: a token stays valid
 * until it expires (`delete` is a no-op), and "sign out everywhere" works by
 * stamping the Auth entity -- tokens issued before the stamp stop validating.
 */
function signedTokenStore(secret: string): CredentialStore {
  const jwt = createJWTHelpers(new TextEncoder().encode(secret), 'HS256')
  return {
    async create(record) {
      const token = await jwt.createJWT(
        { authId: record.authId, signedInBy: record.signedInBy },
        { expiresIn: new TimeSpan(Math.max(1, Math.floor((record.expiresAt.getTime() - record.issuedAt.getTime()) / 1000)), 's') },
      )
      return { id: token }
    },
    async get(id) {
      // The helper verifies the signature and expiry and returns the payload;
      // `iat` / `exp` are the standard claims, in seconds.
      const payload = await jwt
        .validateJWT<{ authId?: unknown; signedInBy?: unknown; iat?: unknown; exp?: unknown }>(id)
        .catch(() => null)
      if (payload === null) {
        return null
      }
      if (typeof payload.authId !== 'string' || typeof payload.signedInBy !== 'string') {
        return null
      }
      const issuedAt = typeof payload.iat === 'number' ? new Date(payload.iat * 1000) : new Date(0)
      const expiresAt = typeof payload.exp === 'number' ? new Date(payload.exp * 1000) : new Date(0)
      const invalidatedAt = await credentialsInvalidatedAt(payload.authId)
      if (invalidatedAt !== null && issuedAt < invalidatedAt) {
        return null
      }
      return {
        authId: payload.authId,
        signedInBy: payload.signedInBy,
        issuedAt,
        expiresAt,
      } satisfies CredentialRecord
    },
    async delete() {},
    async deleteAllForAuthId(authId) {
      await prisma.{= authEntityLower =}.update({ where: { id: authId }, data: { credentialsInvalidatedAt: new Date() } })
    },
  }
}

async function credentialsInvalidatedAt(authId: string): Promise<Date | null> {
  const auth = await prisma.{= authEntityLower =}.findUnique({ where: { id: authId }, select: { credentialsInvalidatedAt: true } })
  return auth?.credentialsInvalidatedAt ?? null
}

// ---- helpers ----------------------------------------------------------------

function parseTimeSpan(ttl: string): TimeSpan {
  const match = /^(\d+)\s*(ms|s|m|h|d|w)$/.exec(ttl.trim())
  if (match === null) {
    throw new Error(`Invalid credential ttl '${ttl}'. Use a number with a unit: 30d, 12h, 15m, 30s.`)
  }
  return new TimeSpan(Number(match[1]), match[2] as 'ms' | 's' | 'm' | 'h' | 'd' | 'w')
}

function contractError(code: string, message: string): Error {
  const error = new Error(message) as Error & { code: string }
  error.code = code
  return error
}
