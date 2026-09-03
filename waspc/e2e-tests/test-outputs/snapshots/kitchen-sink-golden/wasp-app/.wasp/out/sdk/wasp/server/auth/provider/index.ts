import { canManageSessions as canProviderManageSessions, canRevokeSessions as canProviderRevokeSessions, type AuthProvider } from './types.js'
import type { AuthProviderId } from '../../../auth/provider.js'
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
import { createServerAdapter as createServerAdapter_0 } from '@wasp.sh/auth/server'
import { discordConfig as authProviderExtension_0_discordConfigFn } from 'virtual:wasp/user/features/auth/providers/discord'
import { discordUserSignupFields as authProviderExtension_0_discordUserSignupFields } from 'virtual:wasp/user/features/auth/providers/discord'
import { emailUserSignupFields as authProviderExtension_0_emailUserSignupFields } from 'virtual:wasp/user/features/auth/providers/email'
import { getPasswordResetEmailContent as authProviderExtension_0_getPasswordResetEmailContent } from 'virtual:wasp/user/features/auth/providers/email'
import { getVerificationEmailContent as authProviderExtension_0_getVerificationEmailContent } from 'virtual:wasp/user/features/auth/providers/email'
import { gitHubConfig as authProviderExtension_0_githubConfigFn } from 'virtual:wasp/user/features/auth/providers/github'
import { gitHubUserSignupFields as authProviderExtension_0_githubUserSignupFields } from 'virtual:wasp/user/features/auth/providers/github'
import { googleConfig as authProviderExtension_0_googleConfigFn } from 'virtual:wasp/user/features/auth/providers/google'
import { googleUserSignupFields as authProviderExtension_0_googleUserSignupFields } from 'virtual:wasp/user/features/auth/providers/google'
import { microsoftConfig as authProviderExtension_0_microsoftConfigFn } from 'virtual:wasp/user/features/auth/providers/microsoft'
import { microsoftUserSignupFields as authProviderExtension_0_microsoftUserSignupFields } from 'virtual:wasp/user/features/auth/providers/microsoft'
import { onAfterEmailVerified as authProviderExtension_0_onAfterEmailVerified } from 'virtual:wasp/user/features/auth/hooks'
import { slackConfig as authProviderExtension_0_slackConfigFn } from 'virtual:wasp/user/features/auth/providers/slack'
import { slackUserSignupFields as authProviderExtension_0_slackUserSignupFields } from 'virtual:wasp/user/features/auth/providers/slack'

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
 * that provider's id. It is an adapter's *only* window into the app: adapters
 * never import generated code and never read `process.env` themselves, which
 * is what lets them version independently of any app. Wasp's own auth is an
 * adapter package like any other and runs on exactly this. `provision` routes
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

/**
 * The adapter package's server factory for 'wasp', called with
 * everything it may know about the app.
 */
const serverAdapter_0 = await Promise.resolve(
  createServerAdapter_0(
    // The cast narrows the built runtime to the grants the factory's type
    // declares; the generator wired exactly the manifest's `uses`, and the
    // boot assert keeps manifest and adapter honest.
    makeAdapterRuntime({
      providerId: 'wasp',
      serverEnvVarNames: ['JWT_SECRET', 'SKIP_EMAIL_VERIFICATION_IN_DEV', 'GOOGLE_CLIENT_ID', 'GOOGLE_CLIENT_SECRET', 'GITHUB_CLIENT_ID', 'GITHUB_CLIENT_SECRET', 'SLACK_CLIENT_ID', 'SLACK_CLIENT_SECRET', 'DISCORD_CLIENT_ID', 'DISCORD_CLIENT_SECRET', 'MICROSOFT_CLIENT_ID', 'MICROSOFT_CLIENT_SECRET', 'MICROSOFT_TENANT_ID'],
      uses: ['wasp-sessions', 'identity-namespaces', 'email-send'],
      identityNamespaces: ['wasp', 'wasp:email', 'wasp:google', 'wasp:github', 'wasp:slack', 'wasp:discord', 'wasp:microsoft'],
    }) as Parameters<typeof createServerAdapter_0>[0],
    {"onAuthSucceededRedirectTo":"/","clientOAuthCallbackPath":"/oauth/callback","routesBasePath":"/auth/wasp","methods":{"email":{"fromField":{"name":"Wasp Kitchen Sink","email":"kitchen-sink@wasp.sh"},"emailVerificationClientRoute":"/email-verification-","passwordResetClientRoute":"/password-reset"},"google":{"requiredScopes":["profile"]},"github":{"requiredScopes":[]},"slack":{"requiredScopes":["openid"]},"discord":{"requiredScopes":["identify"]},"microsoft":{"requiredScopes":["openid","profile","email"]}}},
    {
      // The user's setup function for the adapter's underlying library; the
      // adapter calls it with its integration config and uses the result.
      setupFn: undefined,
      // Every other user function the manifest referenced, under the name
      // the adapter expects.
      'discordConfigFn': authProviderExtension_0_discordConfigFn,
      'discordUserSignupFields': authProviderExtension_0_discordUserSignupFields,
      'emailUserSignupFields': authProviderExtension_0_emailUserSignupFields,
      'getPasswordResetEmailContent': authProviderExtension_0_getPasswordResetEmailContent,
      'getVerificationEmailContent': authProviderExtension_0_getVerificationEmailContent,
      'githubConfigFn': authProviderExtension_0_githubConfigFn,
      'githubUserSignupFields': authProviderExtension_0_githubUserSignupFields,
      'googleConfigFn': authProviderExtension_0_googleConfigFn,
      'googleUserSignupFields': authProviderExtension_0_googleUserSignupFields,
      'microsoftConfigFn': authProviderExtension_0_microsoftConfigFn,
      'microsoftUserSignupFields': authProviderExtension_0_microsoftUserSignupFields,
      'onAfterEmailVerified': authProviderExtension_0_onAfterEmailVerified,
      'slackConfigFn': authProviderExtension_0_slackConfigFn,
      'slackUserSignupFields': authProviderExtension_0_slackUserSignupFields,
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
  'wasp': serverAdapter_0.provider,
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
export const authProviderRouteHandlers: Partial<Record<AuthProviderId, (req: import('node:http').IncomingMessage, res: import('node:http').ServerResponse) => void | Promise<void>>> = {
  'wasp': serverAdapter_0.routeHandler,
}

/**
 * Each manifest in `main.wasp.ts` made compile-time claims about its provider
 * (its id, its capabilities), and code was generated from them. Checking the
 * claims against the adapter objects at boot turns a wrong manifest into a
 * loud startup failure instead of a subtly broken app.
 */
function assertProvidersMatchManifests(): void {
  const manifests: Array<{ providerId: string; capabilities: string[]; uses: string[] }> = [
    { providerId: 'wasp', capabilities: [], uses: ['wasp-sessions', 'identity-namespaces', 'email-send'] },
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
    if (manifest.providerId.length === 0 || manifest.providerId.includes(':')) {
      errors.push(
        `the manifest declares id '${manifest.providerId}', which is empty or contains a ':' -- ` +
          `the ':' separates a provider id from its identity namespaces`,
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
