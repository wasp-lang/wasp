import type { AuthHandler, Credentials, ProviderIdentities, Subject, WaspEmail, WaspServerRuntime } from './handler/types.js'
import type { AuthSchemeName } from '../../auth/scheme.js'
import { computeSchemeUserFields, provisionAuthUser } from './session.js'
import { getIdentityStore } from './identityStore.js'
import { createIssuer, signOutEverywhere, type IssuerOptions } from './issuer.js'
import { findAuthWithUserBy, type ProviderId } from './utils.js'
import {
  fireVetoableHook,
  onAfterLoginHook,
  mergeUsersFn,
  onAfterLinkHook,
  onBeforeLinkHook,
  onAfterSignupHook,
  onBeforeLoginHook,
  onBeforeSignupHook,
} from './hookDispatch.js'
import { config, prisma } from '../index.js'
import { env as validatedEnv } from '../env.js'
import { emailSender } from '../email/index.js'
import { createServerAuthHandler as createServerAuthHandler_0 } from '@wasp.sh/auth/server'
import { discordConfig as authSchemeExtension_0_discordConfigFn } from 'virtual:wasp/user/features/auth/providers/discord'
import { discordUserSignupFields as authSchemeExtension_0_discordUserSignupFields } from 'virtual:wasp/user/features/auth/providers/discord'
import { emailUserSignupFields as authSchemeExtension_0_emailUserSignupFields } from 'virtual:wasp/user/features/auth/providers/email'
import { getPasswordResetEmailContent as authSchemeExtension_0_getPasswordResetEmailContent } from 'virtual:wasp/user/features/auth/providers/email'
import { getVerificationEmailContent as authSchemeExtension_0_getVerificationEmailContent } from 'virtual:wasp/user/features/auth/providers/email'
import { gitHubConfig as authSchemeExtension_0_githubConfigFn } from 'virtual:wasp/user/features/auth/providers/github'
import { gitHubUserSignupFields as authSchemeExtension_0_githubUserSignupFields } from 'virtual:wasp/user/features/auth/providers/github'
import { googleConfig as authSchemeExtension_0_googleConfigFn } from 'virtual:wasp/user/features/auth/providers/google'
import { googleUserSignupFields as authSchemeExtension_0_googleUserSignupFields } from 'virtual:wasp/user/features/auth/providers/google'
import { microsoftConfig as authSchemeExtension_0_microsoftConfigFn } from 'virtual:wasp/user/features/auth/providers/microsoft'
import { microsoftUserSignupFields as authSchemeExtension_0_microsoftUserSignupFields } from 'virtual:wasp/user/features/auth/providers/microsoft'
import { onAfterEmailVerified as authSchemeExtension_0_onAfterEmailVerified } from 'virtual:wasp/user/features/auth/hooks'
import { slackConfig as authSchemeExtension_0_slackConfigFn } from 'virtual:wasp/user/features/auth/providers/slack'
import { slackUserSignupFields as authSchemeExtension_0_slackUserSignupFields } from 'virtual:wasp/user/features/auth/providers/slack'

/**
 * The scheme registry: every scheme declared in `main.wasp.ts`, instantiated
 * from its handler with the runtime window Wasp hands it. Schemes are built
 * in dependency order, so a scheme's credentials target exists before the
 * scheme that signs into it.
 */

// PRIVATE API
export const defaultScheme: AuthSchemeName = 'wasp'

/**
 * The runtime window a handler gets, with the identity store pre-bound to
 * its scheme name. It is a handler's *only* window into the app: handlers
 * never import generated code and never read `process.env` themselves, which
 * is what lets them version independently of any app. `provision` routes
 * through that scheme's `userSignupFields`, exactly like just-in-time
 * provisioning at first authentication. The casts are the runtime boundary:
 * the store speaks `unknown`, the contract speaks `JsonValue`, and both sides
 * of every value are plain parsed JSON.
 */
type SchemeRuntimeSpec = {
  scheme: AuthSchemeName
  /** Env var names the manifest declared; the runtime env carries exactly these. */
  serverEnvVarNames: readonly string[]
  /** Runtime grants the manifest requested; only these facets get wired. */
  uses: readonly string[]
  /** Identity namespaces the manifest declared (always includes the name). */
  identityNamespaces: readonly string[]
}

/**
 * Errors the granted facets reject with carry a `code` rather than being a
 * class: handler packages hold their own copy of the contract, and
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
 * is what makes acting on another scheme's user unrepresentable through the
 * granted facets.
 */
function resolveOwnNamespace(spec: SchemeRuntimeSpec, namespace: string | undefined): string {
  const resolved = namespace ?? spec.scheme
  if (!spec.identityNamespaces.includes(resolved)) {
    throw contractError(
      'wasp-auth/undeclared-namespace',
      `Auth scheme '${spec.scheme}' tried to use the identity namespace '${resolved}', which its manifest does not declare.`,
    )
  }
  return resolved
}

/** The contract-shaped identity facet for one of the scheme's namespaces. */
function makeIdentitiesFacet(spec: SchemeRuntimeSpec, namespace: string): ProviderIdentities {
  const store = getIdentityStore(namespace)
  return {
    find: (subjectId) => store.find(subjectId) as any,
    provision: (subjectId, identity) =>
      provisionAuthUser(spec.scheme, subjectId, identity?.claims, {
        data: identity?.data,
        secrets: identity?.secrets,
      }, namespace),
    create: async (subjectId, identity, getUserFields, opts) => {
      // The app's signup veto fires FIRST -- at this Wasp-owned choke point no
      // handler can forget it -- and only then do any user-supplied field
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
          : await computeSchemeUserFields(spec.scheme, identity?.claims)
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
    link: async (subjectId, identity, opts) => {
      await assertSchemeOwnsAccount(spec, opts.authId)
      const existing = await store.find(subjectId)
      if (existing !== null) {
        if (existing.authId === opts.authId) {
          return
        }
        throw linkedElsewhereError(namespace)
      }
      const auth = await findAuthWithUserBy({ id: opts.authId })
      if (auth === null) {
        throw contractError('wasp-auth/identity-not-found', 'The account to link to does not exist.')
      }
      const hookProviderId = makeHookProviderId(namespace, subjectId)
      await fireVetoableHook(() =>
        onBeforeLinkHook({ req: opts.req as any, providerId: hookProviderId, user: auth.user }),
      )
      try {
        await store.linkIdentity(subjectId, identity as any, opts.authId)
      } catch (e) {
        // Lost a race against another link or signup of the same subject.
        if (isUniqueConstraintViolation(e)) {
          throw linkedElsewhereError(namespace)
        }
        throw e
      }
      await onAfterLinkHook({
        req: opts.req as any,
        providerId: hookProviderId,
        user: auth.user,
        oauth: opts.hookContext as any,
      })
    },
    unlink: async (subjectId, opts) => {
      await assertSchemeOwnsAccount(spec, opts.authId)
      const outcome = await store.unlinkIdentity(subjectId, opts.authId)
      if (outcome === 'not-found') {
        throw contractError('wasp-auth/identity-not-found', `The account holds no such identity in namespace '${namespace}'.`)
      }
      if (outcome === 'last-identity') {
        throw contractError('wasp-auth/last-identity', "An account's only identity cannot be unlinked.")
      }
    },
    merge: async ({ fromAuthId, intoAuthId, req }) => {
      if (mergeUsersFn === null) {
        throw contractError('wasp-auth/merging-disabled', 'The app declares no auth.mergeUsers, so accounts cannot be merged.')
      }
      const mergeUsers = mergeUsersFn
      if (fromAuthId === intoAuthId) {
        return
      }
      // The scheme must hold an identity on BOTH accounts: it proved control
      // of each through its own logins, and cannot reach anyone else's users.
      await assertSchemeOwnsAccount(spec, fromAuthId)
      await assertSchemeOwnsAccount(spec, intoAuthId)
      await prisma.$transaction(async (tx) => {
        const from = await tx.auth.findUnique({ where: { id: fromAuthId }, include: { user: true } })
        const into = await tx.auth.findUnique({ where: { id: intoAuthId }, include: { user: true } })
        if (from?.user == null || into?.user == null) {
          throw contractError('wasp-auth/identity-not-found', 'One of the accounts to merge does not exist.')
        }
        // The app goes first, while both rows still exist, so it can
        // re-point whatever `from` owns.
        await mergeUsers({ from: from.user, into: into.user, prisma: tx, req: req as any })
        await tx.authIdentity.updateMany({ where: { authId: fromAuthId }, data: { authId: intoAuthId } })
        // Cascades to the old Auth entity and its credentials. A relation
        // the app forgot to re-point fails here and rolls everything back.
        await tx.user.delete({ where: { id: from.user.id } })
      })
    },
    updateData: (subjectId, updates) => store.updateData(subjectId, updates),
    getSecrets: (subjectId) => store.getSecrets(subjectId) as any,
    setSecrets: (subjectId, secrets) => store.setSecrets(subjectId, secrets),
    deleteUser: (subjectId) => store.deleteUser(subjectId),
  }
}

// Says nothing about the other account on purpose: the caller proved control
// of the identity, not the right to learn who else holds it.
function linkedElsewhereError(namespace: string): Error {
  return contractError(
    'wasp-auth/identity-linked-elsewhere',
    `The identity in namespace '${namespace}' already belongs to another account.`,
  )
}

/**
 * The account-linking guard: a scheme may only attach identities to (or
 * detach them from) an account that already carries an identity in one of
 * its OWN namespaces. Without it, a scheme could attach itself to another
 * scheme's users.
 */
async function assertSchemeOwnsAccount(spec: SchemeRuntimeSpec, authId: string): Promise<void> {
  const ownedIdentity = await prisma.authIdentity.findFirst({
    where: { authId, providerName: { in: [...spec.identityNamespaces] } },
    select: { providerName: true },
  })
  if (ownedIdentity === null) {
    throw contractError(
      'wasp-auth/identity-not-found',
      `Auth scheme '${spec.scheme}' has no identity on the account it tried to link.`,
    )
  }
}

// The hook payloads speak `ProviderId` ({ providerName, providerUserId }).
function makeHookProviderId(namespace: string, subjectId: string): ProviderId {
  return { providerName: namespace, providerUserId: subjectId }
}

/**
 * The credentials facet a scheme signs in through, bound to a target issuer
 * and to the calling scheme. The namespace guard, the identity lookup and
 * the app's login hooks all run here, BEFORE the target issues anything --
 * so no scheme can skip the app's login policy, and the issuer records the
 * calling scheme as `signedInBy` without ever being told a name to record.
 */
function boundTo(spec: SchemeRuntimeSpec, target: AuthHandler, targetIssuerOptions: IssuerOptions | null): Credentials {
  if (target.signIn === undefined) {
    throw new Error(`Auth scheme '${spec.scheme}' signs into a scheme whose handler cannot issue credentials.`)
  }
  const signInOnTarget = target.signIn.bind(target)
  const resolveSubject = async (subject: Subject) => {
    const namespace = resolveOwnNamespace(spec, subject.namespace)
    const identity = await getIdentityStore(namespace).find(subject.subjectId)
    if (identity === null) {
      throw contractError(
        'wasp-auth/identity-not-found',
        `No identity for the subject in namespace '${namespace}'. Provision it before signing in.`,
      )
    }
    return { namespace, authId: identity.authId }
  }
  return {
    authenticate: (request) => target.authenticate(request),
    signIn: async (subject, opts) => {
      const { namespace, authId } = await resolveSubject(subject)
      const fireHooks = opts?.skipHooks !== true
      const hookProviderId = makeHookProviderId(namespace, subject.subjectId)
      let hookUser: unknown = undefined
      if (fireHooks) {
        const auth = await findAuthWithUserBy({ id: authId })
        if (auth === null) {
          throw contractError('wasp-auth/identity-not-found', 'The subject resolves to an auth entity with no user.')
        }
        hookUser = auth.user
        await fireVetoableHook(() =>
          onBeforeLoginHook({ req: opts?.req as any, providerId: hookProviderId, user: auth.user }),
        )
      }
      const result = await signInOnTarget(
        { namespace, subjectId: subject.subjectId },
        { signedInBy: spec.scheme, req: opts?.req, properties: opts?.properties },
      )
      if (fireHooks) {
        await onAfterLoginHook({
          req: opts?.req as any,
          providerId: hookProviderId,
          user: hookUser as any,
          oauth: opts?.hookContext as any,
        })
      }
      return result
    },
    signOut: (request) => target.signOut?.(request) ?? Promise.resolve({ status: 200, body: { success: true } }),
    signOutEverywhere: async (subject) => {
      const { authId } = await resolveSubject(subject)
      if (targetIssuerOptions !== null) {
        await signOutEverywhere(targetIssuerOptions, authId)
      }
    },
  }
}

/**
 * The `email-send` grant: the app's configured email sender, sender identity
 * included. SMTP credentials never reach the handler -- only the send
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
          'Sending an email through the auth runtime requires a `from` field, because the app declares no emailSender.defaultFrom.',
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

function makeSchemeRuntime(spec: SchemeRuntimeSpec, credentials: Credentials | null): WaspServerRuntime<never, false> {
  return {
    scheme: spec.scheme,
    mountPath: `/auth/${spec.scheme}`,
    db: prisma,
    dbProvider: 'postgresql',
    // Exactly the vars the manifest declared -- read from the VALIDATED env,
    // so `devDefault`s apply -- and framework secrets (DATABASE_URL) stay
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
    isAccountMergingEnabled: mergeUsersFn !== null,
    identities: makeIdentitiesFacet(spec, spec.scheme),
    ...(credentials !== null ? { credentials } : {}),
    // Granted facets: wired only when the manifest requested them, so an
    // undeclared access fails loudly at first use rather than working by
    // accident.
    ...(spec.uses.includes('email-send') ? { email: waspEmailFacet } : {}),
    ...(spec.uses.includes('identity-namespaces')
      ? {
          identityNamespaces: (namespace: string) =>
            makeIdentitiesFacet(spec, resolveOwnNamespace(spec, namespace)),
        }
      : {}),
  }
}

/** The issuer options of every scheme with inline credentials, for signOutEverywhere. */
const issuerOptionsByScheme: Partial<Record<AuthSchemeName, IssuerOptions>> = {}

// Filled in dependency order below; typed as the full map once complete.
const registered: Partial<Record<AuthSchemeName, { handler: AuthHandler; routeHandler?: (req: import('node:http').IncomingMessage, res: import('node:http').ServerResponse) => void | Promise<void> }>> = {}

function handlerOf(name: AuthSchemeName): AuthHandler {
  const handlerParts = registered[name]
  if (handlerParts === undefined) {
    throw new Error(`Auth scheme '${name}' is not built yet; the generator ordered the schemes wrong.`)
  }
  return handlerParts.handler
}


// ---- scheme 'wasp' (@wasp.sh/auth) ----
const spec_0: SchemeRuntimeSpec = {
  scheme: 'wasp',
  serverEnvVarNames: ['JWT_SECRET', 'SKIP_EMAIL_VERIFICATION_IN_DEV', 'GOOGLE_CLIENT_ID', 'GOOGLE_CLIENT_SECRET', 'GITHUB_CLIENT_ID', 'GITHUB_CLIENT_SECRET', 'SLACK_CLIENT_ID', 'SLACK_CLIENT_SECRET', 'DISCORD_CLIENT_ID', 'DISCORD_CLIENT_SECRET', 'MICROSOFT_CLIENT_ID', 'MICROSOFT_CLIENT_SECRET', 'MICROSOFT_TENANT_ID'],
  uses: ['identity-namespaces', 'email-send'],
  identityNamespaces: ['wasp', 'wasp:email', 'wasp:google', 'wasp:github', 'wasp:slack', 'wasp:discord', 'wasp:microsoft'],
}
const issuerOptions_0: IssuerOptions = {
  scheme: 'wasp',
  transport: 'bearer',
  store: 'prisma',
  ttl: '30d',
  secret: (validatedEnv as Record<string, string | undefined>)['WASP_CREDENTIAL_SECRET'],
  loginPath: `${config.frontendUrl}/login`,
}
issuerOptionsByScheme['wasp'] = issuerOptions_0
// The private issuer behind this scheme's inline `credentials`.
const issuer_0 = createIssuer(issuerOptions_0)
const credentials_0 = boundTo(spec_0, issuer_0, issuerOptions_0)
const handlerParts_0 = await Promise.resolve(
  createServerAuthHandler_0(
    // The cast narrows the built runtime to the grants the factory's type
    // declares; the generator wired exactly the manifest's `uses`, and the
    // boot assert keeps manifest and handler honest.
    makeSchemeRuntime(spec_0, credentials_0) as Parameters<typeof createServerAuthHandler_0>[0],
    {"onAuthSucceededRedirectTo":"/","clientOAuthCallbackPath":"/oauth/callback","methods":{"email":{"fromField":{"name":"Wasp Kitchen Sink","email":"kitchen-sink@wasp.sh"},"emailVerificationClientRoute":"/email-verification-","passwordResetClientRoute":"/password-reset"},"google":{"requiredScopes":["profile"]},"github":{"requiredScopes":[]},"slack":{"requiredScopes":["openid"]},"discord":{"requiredScopes":["identify"]},"microsoft":{"requiredScopes":["openid","profile","email"]}}},
    {
      // The user's setup function for the handler's underlying library; the
      // handler calls it with its integration config and uses the result.
      setupFn: undefined,
      // Every other user function the manifest referenced, under the name
      // the handler expects.
      'discordConfigFn': authSchemeExtension_0_discordConfigFn,
      'discordUserSignupFields': authSchemeExtension_0_discordUserSignupFields,
      'emailUserSignupFields': authSchemeExtension_0_emailUserSignupFields,
      'getPasswordResetEmailContent': authSchemeExtension_0_getPasswordResetEmailContent,
      'getVerificationEmailContent': authSchemeExtension_0_getVerificationEmailContent,
      'githubConfigFn': authSchemeExtension_0_githubConfigFn,
      'githubUserSignupFields': authSchemeExtension_0_githubUserSignupFields,
      'googleConfigFn': authSchemeExtension_0_googleConfigFn,
      'googleUserSignupFields': authSchemeExtension_0_googleUserSignupFields,
      'microsoftConfigFn': authSchemeExtension_0_microsoftConfigFn,
      'microsoftUserSignupFields': authSchemeExtension_0_microsoftUserSignupFields,
      'onAfterEmailVerified': authSchemeExtension_0_onAfterEmailVerified,
      'slackConfigFn': authSchemeExtension_0_slackConfigFn,
      'slackUserSignupFields': authSchemeExtension_0_slackUserSignupFields,
    },
  ),
)
registered['wasp'] = handlerParts_0

// PRIVATE API
/**
 * The app's auth schemes, keyed by name. Everything else in Wasp depends on
 * the `AuthHandler` interface rather than on concrete implementations.
 */
export const authSchemes: { readonly [Name in AuthSchemeName]: AuthHandler } = {
  'wasp': handlerOf('wasp'),
}

// PRIVATE API
export function getAuthScheme(name: string): AuthHandler | undefined {
  return (authSchemes as Record<string, AuthHandler>)[name]
}

// PRIVATE API
/**
 * Node handlers for the routes schemes brought with them, keyed by scheme
 * name. The server mounts each at `/auth/<scheme>`.
 */
export const authSchemeRouteHandlers: Partial<Record<AuthSchemeName, (req: import('node:http').IncomingMessage, res: import('node:http').ServerResponse) => void | Promise<void>>> = {
  'wasp': handlerParts_0.routeHandler,
}

/**
 * Each manifest in `main.wasp.ts` made compile-time claims about its handler
 * (its capabilities), and code was generated from them. Checking the claims
 * against the handler objects at boot turns a wrong manifest into a loud
 * startup failure instead of a subtly broken app.
 */
function assertHandlersMatchManifests(): void {
  const manifests: Array<{ scheme: AuthSchemeName; capabilities: string[] }> = [
    { scheme: 'wasp', capabilities: [] },
  ]
  const errors: string[] = []
  for (const manifest of manifests) {
    const handler = authSchemes[manifest.scheme]
    if (typeof handler.authenticate !== 'function') {
      errors.push(`the handler of scheme '${manifest.scheme}' has no authenticate method`)
    }
    if (manifest.capabilities.includes('sign-in') && typeof handler.signIn !== 'function') {
      errors.push(
        `the manifest of scheme '${manifest.scheme}' declares the 'sign-in' capability, but its handler has no signIn method`,
      )
    }
  }
  if (errors.length > 0) {
    throw new Error('Auth handlers do not match their manifests:\n' + errors.map((error) => `  - ${error}`).join('\n'))
  }
}

assertHandlersMatchManifests()
