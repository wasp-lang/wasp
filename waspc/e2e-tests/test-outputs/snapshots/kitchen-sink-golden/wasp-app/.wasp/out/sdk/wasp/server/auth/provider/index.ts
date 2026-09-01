import { type AuthProvider } from './types.js'
import type { AuthProviderId, ExternalAuthProviderId } from '../../../auth/provider.js'
import type { ProviderIdentities, WaspEmail, WaspServerRuntime, WaspSessions } from '@wasp.sh/auth-contract'
import { computeProviderUserFields, provisionAuthUser } from '../session.js'
import { getIdentityStore } from '../identityStore.js'
import * as sessionStore from '../sessionStore.js'
import { findAuthWithUserBy, type ProviderId } from '../utils.js'
import {
  fireVetoableHook,
  onAfterLoginHook,
  onAfterSignupHook,
  onBeforeLoginHook,
  onBeforeSignupHook,
} from '../hookDispatch.js'
import { config, prisma } from '../../index.js'
import { env as validatedEnv } from '../../env.js'
import { emailSender } from '../../email/index.js'
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
 * The runtime window a provider gets, with the identity store pre-bound to
 * that provider's id. For adapter packages it is their *only* window into the
 * app: adapters never import generated code and never read `process.env`
 * themselves, which is what lets them version independently of any app.
 * Wasp's own auth runs on the very same runtime (see `waspAuthRuntime`), so
 * its flows hold no powers an adapter cannot request. `provision` routes
 * through that provider's `userSignupFields`, exactly like just-in-time
 * provisioning at the login exchange. The casts are the runtime boundary: the
 * store speaks `unknown`, the contract speaks `JsonValue`, and both sides of
 * every value are plain parsed JSON.
 */
type AdapterRuntimeSpec = {
  providerId: AuthProviderId
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
    create: async (subjectId, identity, getUserFields, opts) => {
      // The app's signup veto fires FIRST -- at this Wasp-owned choke point no
      // provider can forget it -- and only then do any user-supplied field
      // getters run (that ordering is why `getUserFields` is a lazy callback).
      if (opts?.skipHooks !== true) {
        await fireVetoableHook(() =>
          onBeforeSignupHook({
            req: opts?.req as any,
            providerId: makeHookProviderId(namespace, subjectId),
          }),
        )
      }
      const userFields =
        getUserFields !== undefined
          ? await getUserFields()
          : await computeProviderUserFields(spec.providerId, identity?.claims)
      let created
      try {
        created = await store.createIdentity(subjectId, identity as any, userFields as any)
      } catch (e) {
        if (isUniqueConstraintViolation(e)) {
          throw contractError(
            'wasp-auth/duplicate-identity',
            `An identity for this subject already exists in namespace '${namespace}'.`,
          )
        }
        throw e
      }
      if (opts?.skipHooks !== true) {
        await onAfterSignupHook({
          req: opts?.req as any,
          providerId: makeHookProviderId(namespace, subjectId),
          user: created,
          oauth: opts?.hookContext as any,
        })
      }
      return { authId: created.auth!.id }
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
      // The app's login hooks fire around every mint at this Wasp-owned choke
      // point (veto by throwing), whichever provider is minting. `skipHooks`
      // exists for flows that already fired them at a more informative moment
      // (wasp-auth's OAuth callback holds the tokens; the redeem route does
      // not).
      const fireHooks = opts?.skipHooks !== true
      const hookProviderId = makeHookProviderId(
        resolveOwnNamespace(spec, subject.namespace),
        subject.subjectId,
      )
      let hookUser: unknown = undefined
      if (fireHooks) {
        const auth = await findAuthWithUserBy({ id: authId })
        if (auth === null) {
          throw contractError(
            'wasp-auth/identity-not-found',
            'The subject resolves to an auth entity with no user.',
          )
        }
        hookUser = auth.user
        await fireVetoableHook(() =>
          onBeforeLoginHook({
            req: opts?.req as any,
            providerId: hookProviderId,
            user: auth.user,
          }),
        )
      }
      const session = await sessionStore.createSession(authId, {
        providerId: spec.providerId,
        providerSessionId: opts?.providerSessionId,
      })
      if (fireHooks) {
        await onAfterLoginHook({
          req: opts?.req as any,
          providerId: hookProviderId,
          user: hookUser as any,
          oauth: opts?.hookContext as any,
        })
      }
      return { sessionId: session.id }
    },
    revoke: (sessionId) => sessionStore.revokeSession(sessionId),
    revokeAllForSubject: async (subject) => {
      const authId = await resolveSubjectAuthId(subject)
      await sessionStore.revokeAllSessions(authId)
    },
  }
}

// The hook payloads speak `ProviderId` ({ providerName, providerUserId });
// external namespaces are not in the generated `ProviderName` union, so the
// cast widens it -- the values are plain strings either way.
function makeHookProviderId(namespace: string, subjectId: string): ProviderId {
  return { providerName: namespace, providerUserId: subjectId } as ProviderId
}

/**
 * The `email-send` grant: the app's configured email sender, sender identity
 * included. SMTP credentials never reach the adapter -- only the send
 * capability does.
 */
const waspEmailFacet: WaspEmail = (() => {
  // Aeson encodes an absent name as null; the contract speaks `name?: string`.
  const configured: { email: string; name?: string | null } | undefined = {"email":"kitchen-sink@wasp.sh","name":null}
  const defaultFrom =
    configured === undefined
      ? undefined
      : { email: configured.email, ...(configured.name ? { name: configured.name } : {}) }
  return {
    defaultFrom,
    send: async (email) => {
      const from = email.from ?? defaultFrom
      if (from === undefined) {
        throw new Error(
          'Sending an email through the auth provider runtime requires a `from` field, because the app declares no emailSender.defaultFrom.',
        )
      }
      await emailSender.send({
        from,
        to: email.to,
        subject: email.subject,
        text: email.text,
        html: email.html,
      })
    },
  }
})()

function makeAdapterRuntime(spec: AdapterRuntimeSpec): WaspServerRuntime<never> {
  return {
    db: prisma,
    dbProvider: 'postgresql',
    // Exactly the vars the manifest declared -- read from the VALIDATED env,
    // so `devDefault`s apply -- and framework secrets (JWT_SECRET) stay
    // unreachable (declaring a framework-owned name is a compile error).
    env: Object.fromEntries(
      spec.serverEnvVarNames.map((name) => [
        name,
        (validatedEnv as Record<string, string | undefined>)[name],
      ]),
    ),
    serverUrl: config.serverUrl,
    clientUrl: config.frontendUrl,
    isDevelopment: config.isDevelopment,
    identities: makeIdentitiesFacet(spec, spec.providerId),
    // Granted facets: wired only when the manifest requested them, so an
    // undeclared access fails loudly at first use rather than working by
    // accident.
    ...(spec.uses.includes('wasp-sessions') ? { sessions: makeSessionsFacet(spec) } : {}),
    ...(spec.uses.includes('email-send') ? { email: waspEmailFacet } : {}),
    ...(spec.uses.includes('identity-namespaces')
      ? {
          identityNamespaces: (namespace: string) =>
            makeIdentitiesFacet(spec, resolveOwnNamespace(spec, namespace)),
        }
      : {}),
  }
}

// PRIVATE API
/**
 * Wasp's own auth, running on the very same runtime window an adapter package
 * gets: sessions through the `wasp-sessions` grant, identities through
 * namespace facets (one per enabled method), email through the `email-send`
 * grant. Its only manifest-level privileges are compatibility-shaped -- the
 * unprefixed method namespaces and reading its configuration from the
 * generated env schema instead of a declared env list.
 */
export const waspAuthRuntime = makeAdapterRuntime({
  providerId: 'wasp',
  serverEnvVarNames: [],
  uses: ['wasp-sessions', 'identity-namespaces', 'email-send'],
  identityNamespaces: ['wasp', 'email', 'google', 'github', 'slack', 'discord', 'microsoft'],
}) as WaspServerRuntime<'wasp-sessions' | 'identity-namespaces'>

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
}

// PRIVATE API
/**
 * The external providers a credential can be exchanged with (`POST
 * /auth/login/:providerId`). Deliberately excludes 'wasp': Wasp's own auth
 * mints sessions through its own routes, and exchanging a Wasp credential for
 * a Wasp session would be a loop.
 */
export const externalAuthProviders: { readonly [Id in ExternalAuthProviderId]: AuthProvider } = {
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
}
