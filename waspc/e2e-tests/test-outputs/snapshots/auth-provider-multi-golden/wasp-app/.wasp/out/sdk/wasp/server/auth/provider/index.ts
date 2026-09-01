import { canManageSessions as canProviderManageSessions, canRevokeSessions as canProviderRevokeSessions, type AuthProvider } from './types.js'
import type { AuthProviderId, ExternalAuthProviderId } from '../../../auth/provider.js'
import type { ProviderIdentities, WaspEmail, WaspServerRuntime, WaspSessions } from '@wasp.sh/auth-contract'
import { computeProviderUserFields, provisionAuthUser } from '../session.js'
import { getIdentityStore } from '../identityStore.js'
import * as sessionStore from '../sessionStore.js'
import { config, prisma } from '../../index.js'
import { createServerAdapter as createServerAdapter_0 } from '@wasp.sh/auth-clerk/server'
import { waspAuthProvider } from './wasp.js'

// PRIVATE API
export {
  type AuthProvider,
  type SessionManagingAuthProvider,
  type SupportsSessionRevocation,
  type VerifiedSession,
  canManageSessions,
  canRevokeSessions,
} from './types.js'

/**
 * The runtime window an adapter package gets, with the identity store
 * pre-bound to that provider's id. This runtime object is the adapter's *only*
 * window into the app: adapters never import generated code and never read
 * `process.env` themselves, which is what lets them version independently of
 * any app. `provision` routes through that provider's `userSignupFields`,
 * exactly like just-in-time provisioning at the login exchange. The casts are
 * the runtime boundary: the store speaks `unknown`, the contract speaks
 * `JsonValue`, and both sides of every value are plain parsed JSON.
 */
type AdapterRuntimeSpec = {
  providerId: ExternalAuthProviderId
  /** Env var names the manifest declared; the runtime env carries exactly these. */
  serverEnvVarNames: readonly string[]
  /** Runtime grants the manifest requested; only these facets get wired. */
  uses: readonly string[]
  /** Identity namespaces the manifest declared (always includes the id). */
  identityNamespaces: readonly string[]
}

/**
 * Errors the granted facets reject with carry a `code` rather than being a
 * class: adapter packages hold their own copy of the contract, and
 * `instanceof` does not survive package-copy boundaries.
 */
function contractError(code: string, message: string): Error {
  const error = new Error(message) as Error & { code: string }
  error.code = code
  return error
}

function isUniqueConstraintViolation(e: unknown): boolean {
  return (
    typeof e === 'object' && e !== null && 'code' in e && (e as { code: unknown }).code === 'P2002'
  )
}

/**
 * The namespace-membership guard, run BEFORE any store access: the identity
 * store itself resolves any namespace string, so this check (not the lookup)
 * is what makes acting on another provider's user unrepresentable through the
 * granted facets.
 */
function resolveOwnNamespace(spec: AdapterRuntimeSpec, namespace: string | undefined): string {
  const resolved = namespace ?? spec.providerId
  if (!spec.identityNamespaces.includes(resolved)) {
    throw contractError(
      'wasp-auth/undeclared-namespace',
      `Auth provider '${spec.providerId}' tried to use the identity namespace '${resolved}', which its manifest does not declare.`,
    )
  }
  return resolved
}

/** The contract-shaped identity facet for one of the provider's namespaces. */
function makeIdentitiesFacet(spec: AdapterRuntimeSpec, namespace: string): ProviderIdentities {
  const store = getIdentityStore(namespace)
  return {
    find: (subjectId) => store.find(subjectId) as any,
    provision: (subjectId, identity) =>
      provisionAuthUser(spec.providerId, subjectId, identity?.claims, {
        data: identity?.data,
        secrets: identity?.secrets,
      }, namespace),
    create: async (subjectId, identity, getUserFields) => {
      // The lazy callback is what lets the provisioning layer order the app's
      // signup veto before any user-supplied field getters run.
      const userFields =
        getUserFields !== undefined
          ? await getUserFields()
          : await computeProviderUserFields(spec.providerId, identity?.claims)
      try {
        const created = await store.createIdentity(subjectId, identity as any, userFields as any)
        return { authId: created.auth!.id }
      } catch (e) {
        if (isUniqueConstraintViolation(e)) {
          throw contractError(
            'wasp-auth/duplicate-identity',
            `An identity for this subject already exists in namespace '${namespace}'.`,
          )
        }
        throw e
      }
    },
    updateData: (subjectId, updates) => store.updateData(subjectId, updates),
    getSecrets: (subjectId) => store.getSecrets(subjectId) as any,
    setSecrets: (subjectId, secrets) => store.setSecrets(subjectId, secrets),
    deleteUser: (subjectId) => store.deleteUser(subjectId),
  }
}

/**
 * The `wasp-sessions` grant. Minting is subject-bound (the namespace guard
 * plus the identity lookup), and the session records THIS provider's id --
 * the inputs `authRequired: [...]` enforcement trusts. `revokeAllForSubject`
 * is deliberately the raw store call, NOT the dual-sign-out loop
 * (`invalidateAllSessionsForAuthId` re-enters `provider.revokeSession`, so an
 * adapter calling it from its own revocation path would recurse).
 */
function makeSessionsFacet(spec: AdapterRuntimeSpec): WaspSessions {
  const resolveSubjectAuthId = async (subject: { namespace?: string; subjectId: string }): Promise<string> => {
    const namespace = resolveOwnNamespace(spec, subject.namespace)
    const identity = await getIdentityStore(namespace).find(subject.subjectId)
    if (identity === null) {
      throw contractError(
        'wasp-auth/identity-not-found',
        `No identity for the subject in namespace '${namespace}'. Provision it before minting or revoking sessions.`,
      )
    }
    return identity.authId
  }
  return {
    issue: async (subject, opts) => {
      const authId = await resolveSubjectAuthId(subject)
      const session = await sessionStore.createSession(authId, {
        providerId: spec.providerId,
        providerSessionId: opts?.providerSessionId,
      })
      return { sessionId: session.id }
    },
    revoke: (sessionId) => sessionStore.revokeSession(sessionId),
    revokeAllForSubject: async (subject) => {
      const authId = await resolveSubjectAuthId(subject)
      await sessionStore.revokeAllSessions(authId)
    },
  }
}


function makeAdapterRuntime(spec: AdapterRuntimeSpec): WaspServerRuntime<never> {
  return {
    db: prisma,
    dbProvider: 'sqlite',
    // Exactly the vars the manifest declared: what an adapter reads is what
    // its manifest shows, and framework secrets (JWT_SECRET) stay unreachable
    // (declaring a framework-owned name is a compile error).
    env: Object.fromEntries(
      spec.serverEnvVarNames.map((name) => [name, process.env[name]]),
    ),
    serverUrl: config.serverUrl,
    clientUrl: config.frontendUrl,
    isDevelopment: config.isDevelopment,
    identities: makeIdentitiesFacet(spec, spec.providerId),
    // Granted facets: wired only when the manifest requested them, so an
    // undeclared access fails loudly at first use rather than working by
    // accident.
    ...(spec.uses.includes('wasp-sessions') ? { sessions: makeSessionsFacet(spec) } : {}),
    ...(spec.uses.includes('identity-namespaces')
      ? {
          identityNamespaces: (namespace: string) =>
            makeIdentitiesFacet(spec, resolveOwnNamespace(spec, namespace)),
        }
      : {}),
  }
}

/**
 * The adapter package's server factory for 'external:clerk', called with
 * everything it may know about the app.
 */
const serverAdapter_0 = await Promise.resolve(
  createServerAdapter_0(
    // The cast narrows the built runtime to the grants the factory's type
    // declares; the generator wired exactly the manifest's `uses`, and the
    // boot assert keeps manifest and adapter honest.
    makeAdapterRuntime({
      providerId: 'external:clerk',
      serverEnvVarNames: ['CLERK_SECRET_KEY', 'CLERK_PUBLISHABLE_KEY', 'CLERK_JWT_KEY'],
      uses: [],
      identityNamespaces: ['external:clerk'],
    }) as Parameters<typeof createServerAdapter_0>[0],
    undefined,
    {
      // The user's setup function for the adapter's underlying library; the
      // adapter calls it with its integration config and uses the result.
      setupFn: undefined,
    },
  ),
)

// PRIVATE API
/**
 * The app's auth providers, keyed by provider id, in `main.wasp.ts`
 * declaration order.
 *
 * Everything else in Wasp depends on the `AuthProvider` interface rather than
 * on concrete implementations. Every provider a session can name is here, so
 * looking up a session's minting provider always succeeds.
 */
export const authProviders: { readonly [Id in AuthProviderId]: AuthProvider } = {
  'wasp': waspAuthProvider,
  'external:clerk': serverAdapter_0.provider,
}

// PRIVATE API
/**
 * The external providers a credential can be exchanged with (`POST
 * /auth/login/:providerId`). Deliberately excludes 'wasp': Wasp's own auth
 * mints sessions through its own routes, and exchanging a Wasp credential for
 * a Wasp session would be a loop.
 */
export const externalAuthProviders: { readonly [Id in ExternalAuthProviderId]: AuthProvider } = {
  'external:clerk': authProviders['external:clerk'],
}

// PRIVATE API
export function getAuthProvider(providerId: string): AuthProvider | undefined {
  return (authProviders as Record<string, AuthProvider>)[providerId]
}

// PRIVATE API
/**
 * Node handlers for the routes external providers brought with them, keyed by
 * provider id. The server mounts each at the basePath its manifest declared.
 */
export const authProviderRouteHandlers: Partial<Record<ExternalAuthProviderId, (req: import('node:http').IncomingMessage, res: import('node:http').ServerResponse) => void | Promise<void>>> = {
  'external:clerk': serverAdapter_0.routeHandler,
}

/**
 * Each manifest in `main.wasp.ts` made compile-time claims about its provider
 * (its id, its capabilities), and code was generated from them. Checking the
 * claims against the adapter objects at boot turns a wrong manifest into a
 * loud startup failure instead of a subtly broken app.
 */
function assertProvidersMatchManifests(): void {
  const manifests: Array<{ providerId: string; capabilities: string[]; uses: string[] }> = [
    { providerId: 'external:clerk', capabilities: ['session-revocation'], uses: [] },
  ]
  const knownRuntimeGrants = ['wasp-sessions', 'email-send', 'identity-namespaces']

  const errors: string[] = []

  for (const manifest of manifests) {
    const provider = getAuthProvider(manifest.providerId)
    if (provider === undefined) {
      continue
    }

    // Both rules below are compile-time errors too (mapper + Haskell
    // validator); asserting them here as well means no generated-code path can
    // quietly outlive a validation gap.
    if (!manifest.providerId.startsWith('external:')) {
      errors.push(
        `the manifest declares id '${manifest.providerId}', which does not start with 'external:' -- ` +
          `the unprefixed namespace is reserved for Wasp's own auth methods`,
      )
    }

    if (
      manifest.capabilities.includes('cookie-transport') &&
      !manifest.capabilities.includes('session-revocation')
    ) {
      errors.push(
        `the manifest for '${manifest.providerId}' declares 'cookie-transport' without 'session-revocation' -- ` +
          `a cookie-borne credential Wasp cannot revoke server-side would make logout() a lie`,
      )
    }

    for (const grant of manifest.uses) {
      if (!knownRuntimeGrants.includes(grant)) {
        errors.push(
          `the manifest for '${manifest.providerId}' requests the unknown runtime grant '${grant}' -- ` +
            `the generator could not have wired it`,
        )
      }
    }

    if (provider.id !== manifest.providerId) {
      errors.push(
        `the manifest declares id '${manifest.providerId}', but the adapter's id is '${provider.id}' -- ` +
          `identities are recorded under the provider id, so the two must match`,
      )
    }

    if (
      manifest.capabilities.includes('issue-sessions') &&
      !canProviderManageSessions(provider)
    ) {
      errors.push(
        `the manifest for '${manifest.providerId}' declares the 'issue-sessions' capability, but the adapter does not implement the full ` +
          `issueSession/revokeSession/revokeAllSessions set Wasp requires for session management`,
      )
    }

    if (
      manifest.capabilities.includes('session-revocation') &&
      !canProviderRevokeSessions(provider)
    ) {
      errors.push(
        `the manifest for '${manifest.providerId}' declares the 'session-revocation' capability, but the adapter does not implement revokeSession`,
      )
    }
  }

  if (errors.length > 0) {
    throw new Error(
      'Auth provider adapters do not match their manifests:\n' +
        errors.map((error) => `  - ${error}`).join('\n'),
    )
  }
}

assertProvidersMatchManifests()
