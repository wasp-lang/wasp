import type { IncomingMessage } from 'node:http'
import { TimeSpan, createJWTHelpers } from '@wasp.sh/lib-auth/node'
import type {
  AuthHandler,
  AuthResponse,
  CredentialRecord,
  CredentialStore,
  SignInContext,
  SignInResult,
  Subject,
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
  /** Signing secret; required by the signed-token store. */
  secret?: string
  /** Where a browser navigation with no credential is sent (cookie transport). */
  loginPath: string
}

const COOKIE_NAME = 'wasp_credential'

// PRIVATE API
export function createIssuer(options: IssuerOptions): AuthHandler {
  const store = resolveStore(options)
  const transport = options.transport === 'cookie' ? cookieTransport(options) : bearerTransport
  const ttl = parseTimeSpan(options.ttl)

  return {
    async authenticate(request) {
      const id = transport.read(request)
      if (id === null) {
        return { status: 'unauthenticated' }
      }
      const record = await store.get(id)
      if (record === null) {
        return { status: 'unauthenticated' }
      }
      return {
        status: 'authenticated',
        principal: { subjectId: record.authId, signedInBy: record.signedInBy, credentialId: id },
      }
    },

    async signIn(subject: Subject, context: SignInContext): Promise<SignInResult> {
      // The issuer keys its records by the Auth entity id: the subject has
      // already been resolved (and its namespace guarded) by the facet that
      // called in, so this lookup cannot cross scheme boundaries.
      const identity = await getIdentityStore(subject.namespace ?? context.signedInBy).find(subject.subjectId)
      if (identity === null) {
        throw contractError('wasp-auth/identity-not-found', 'No identity for the subject to issue a credential for.')
      }
      const issuedAt = new Date()
      const { id } = await store.create({
        authId: identity.authId,
        signedInBy: context.signedInBy,
        issuedAt,
        expiresAt: new Date(issuedAt.getTime() + ttl.milliseconds()),
      })
      return { response: transport.write(id), credentialId: id }
    },

    async signOut(request) {
      const id = transport.read(request)
      if (id !== null) {
        await store.delete(id)
      }
      return transport.clear()
    },

    challenge: async (request) => transport.challenge(request),
    forbid: async () => ({ status: 403, body: { message: 'Forbidden' } }),
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
  write(id: string): AuthResponse
  clear(): AuthResponse
  challenge(request: Request): AuthResponse
}

const bearerTransport: Transport = {
  read: (request) => {
    const header = request.headers.get('authorization')
    const prefix = 'Bearer '
    return header !== null && header.startsWith(prefix) ? header.substring(prefix.length) : null
  },
  // The generated client stores the credential and attaches it to every request.
  write: (id) => ({ status: 200, body: { credential: id } }),
  clear: () => ({ status: 200, body: { success: true } }),
  challenge: () => ({ status: 401, body: { message: 'Invalid credentials' } }),
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
    write: (id) => ({
      status: 200,
      headers: { 'Set-Cookie': `${COOKIE_NAME}=${encodeURIComponent(id)}; ${attributes}; Max-Age=${parseTimeSpan(options.ttl).seconds()}` },
      body: { success: true },
    }),
    clear: () => ({
      status: 200,
      headers: { 'Set-Cookie': `${COOKIE_NAME}=; ${attributes}; Max-Age=0` },
      body: { success: true },
    }),
    // A browser navigation gets the login page; an API call gets a 401.
    challenge: (request) =>
      (request.headers.get('accept') ?? '').includes('text/html')
        ? { status: 302, headers: { Location: options.loginPath } }
        : { status: 401, body: { message: 'Invalid credentials' } },
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
      await prisma.auth.update({ where: { id: authId }, data: { credentialsInvalidatedAt: new Date() } })
    },
  }
}

async function credentialsInvalidatedAt(authId: string): Promise<Date | null> {
  const auth = await prisma.auth.findUnique({ where: { id: authId }, select: { credentialsInvalidatedAt: true } })
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

// PRIVATE API
/** Applies a handler's response to the Node response. */
export function sendAuthResponse(res: import('node:http').ServerResponse, response: AuthResponse): void {
  res.statusCode = response.status
  for (const [name, value] of Object.entries(response.headers ?? {}) as Array<[string, string | string[]]>) {
    res.setHeader(name, value)
  }
  if (response.body !== undefined) {
    res.setHeader('Content-Type', 'application/json')
    res.end(JSON.stringify(response.body))
  } else {
    res.end()
  }
}

// PRIVATE API
/** A standard web Request built from a Node request: headers and URL only, which is all handlers read. */
export function toWebRequest(req: IncomingMessage & { protocol?: string; originalUrl?: string }): Request {
  const headers = new Headers()
  for (const [key, value] of Object.entries(req.headers)) {
    if (typeof value === 'string') {
      headers.set(key, value)
    } else if (Array.isArray(value)) {
      headers.set(key, value.join(', '))
    }
  }
  const host = req.headers.host ?? 'localhost'
  const protocol = req.protocol ?? 'http'
  return new Request(`${protocol}://${host}${req.originalUrl ?? req.url ?? '/'}`, {
    method: req.method,
    headers,
  })
}
