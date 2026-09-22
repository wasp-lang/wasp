{{={= =}=}}
import type { AuthHandler, CredentialsIssuer, IdentityStore, AuthIdentityRef, OAuthLoginData, WaspEmail, WaspServerRuntime } from './handler/types.js'
import type { OAuthData } from './hooks.js'
import type { AuthSchemeName } from '../../auth/scheme.js'
import { joinHandlerSpec } from '../../auth/handlerSpec.js'
import { computeSchemeUserFields, provisionAuthUser } from './session.js'
import { getIdentityStore } from './identityStore.js'
import {
  createIssuer,
  createOneTimeCode,
  redeemOneTimeCode,
  signOutEverywhere,
  type IssuerOptions,
} from './issuer.js'
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
{=# isEmailSenderEnabled =}
import { emailSender } from '../email/index.js'
{=/ isEmailSenderEnabled =}
{=# schemes =}
{=# isPackage =}
{=^ isFrameworkIssuer =}
import { {= serverExportName =} as createServerAuthHandler_{= index =} } from '{= serverPackage =}'
{=/ isFrameworkIssuer =}
{=/ isPackage =}
{=^ isPackage =}
{=& handlerModule.importStatement =}
{=/ isPackage =}
{=# specReferences =}
{=& import.importStatement =}
{=/ specReferences =}
{=# inlineCredentials =}
{=# storeModule.isDefined =}
{=& storeModule.importStatement =}
{=/ storeModule.isDefined =}
{=/ inlineCredentials =}
{=/ schemes =}

/**
 * The scheme registry: every scheme declared in `main.wasp.ts`, instantiated
 * from its handler with the runtime window Wasp hands it. Schemes are built
 * in dependency order, so a scheme's credentials target exists before the
 * scheme that signs into it.
 */

// PRIVATE API
export const defaultScheme: AuthSchemeName = '{= defaultScheme =}'

/**
 * The runtime window a handler gets, with the identity store pre-bound to
 * its scheme name. It is a handler's *only* window into the app: handlers
 * never import generated code and never read `process.env` themselves, which
 * is what lets them version independently of any app. `provision` routes
 * through that scheme's `userFieldsFromClaims`, exactly like just-in-time
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
  /** The scheme's provider names (`email`); `default` when the manifest declared none. */
  providers: { readonly [providerName: string]: { readonly kind?: string } }
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
 * The provider name membership guard, run BEFORE any store access: the identity
 * store itself resolves any provider name string, so this check (not the lookup)
 * is what makes acting on another scheme's user unrepresentable through the
 * granted facets.
 */
function resolveOwnProviderName(spec: SchemeRuntimeSpec, providerName: string | undefined): string {
  const resolved = providerName ?? 'default'
  if (!Object.hasOwn(spec.providers, resolved)) {
    throw contractError(
      'wasp-auth/undeclared-provider-name',
      `Auth scheme '${spec.scheme}' tried to use the provider name '${resolved}', which its manifest does not declare.`,
    )
  }
  return resolved
}

/**
 * What the app's hooks receive as `oauth`. A provider declared with
 * `kind: "oauth"` must come with its OAuth data on every signup, login and
 * link: a typed adapter cannot leave it out, and this check covers the ones
 * that are not typed. Wasp adds the provider's name.
 */
function resolveOAuthData(
  spec: SchemeRuntimeSpec,
  providerName: string,
  oauth: OAuthLoginData | undefined,
): OAuthData | undefined {
  if (oauth === undefined) {
    if (spec.providers[providerName]?.kind === 'oauth') {
      throw contractError(
        'wasp-auth/missing-oauth-data',
        `Auth scheme '${spec.scheme}' declares the provider '${providerName}' with kind "oauth", so every signup, login and link through it must pass \`oauth\` ({ uniqueRequestId, tokens }).`,
      )
    }
    return undefined
  }
  return { ...oauth, providerName }
}

/** The contract-shaped identity facet for one of the scheme's provider names. */
function makeIdentitiesFacet(spec: SchemeRuntimeSpec, providerName: string): IdentityStore {
  const store = getIdentityStore(spec.scheme, providerName)
  return {
    find: (providerUserId) => store.find(providerUserId) as any,
    provision: async (providerUserId, opts) =>
      provisionAuthUser(spec.scheme, providerUserId, opts?.identity?.claims, {
        data: opts?.identity?.data,
        secrets: opts?.identity?.secrets,
      }, providerName, { req: opts?.req as any, oauth: resolveOAuthData(spec, providerName, opts?.oauth) }),
    create: async (providerUserId, opts) => {
      const { identity, getUserFields } = opts ?? {}
      const oauth = resolveOAuthData(spec, providerName, opts?.oauth)
      // The app's signup veto fires FIRST -- at this Wasp-owned choke point no
      // handler can forget it -- and only then do any user-supplied field
      // getters run (that ordering is why `getUserFields` is a lazy callback).
      if (opts?.skipHooks !== true) {
        await fireVetoableHook(() =>
          onBeforeSignupHook({
            req: opts?.req as any,
            providerId: makeHookProviderId(spec.scheme, providerName, providerUserId),
          }),
        )
      }
      const userFields =
        getUserFields !== undefined
          ? await getUserFields()
          : await computeSchemeUserFields(spec.scheme, identity?.claims)
      let created
      try {
        created = await store.createIdentity(providerUserId, identity as any, userFields as any)
      } catch (e) {
        if (isUniqueConstraintViolation(e)) {
          throw contractError(
            'wasp-auth/duplicate-identity',
            `An identity for this subject already exists in provider name '${providerName}'.`,
          )
        }
        throw e
      }
      if (opts?.skipHooks !== true) {
        await onAfterSignupHook({
          req: opts?.req as any,
          providerId: makeHookProviderId(spec.scheme, providerName, providerUserId),
          user: created,
          oauth,
        })
      }
      return { authId: created.{= authFieldOnUserEntityName =}!.id }
    },
    link: async (providerUserId, opts) => {
      await assertSchemeOwnsAccount(spec, opts.authId)
      const existing = await store.find(providerUserId)
      if (existing !== null) {
        if (existing.authId === opts.authId) {
          return
        }
        throw linkedElsewhereError(providerName)
      }
      const auth = await findAuthWithUserBy({ id: opts.authId })
      if (auth === null) {
        throw contractError('wasp-auth/identity-not-found', 'The account to link to does not exist.')
      }
      const oauth = resolveOAuthData(spec, providerName, opts.oauth)
      const hookProviderId = makeHookProviderId(spec.scheme, providerName, providerUserId)
      await fireVetoableHook(() =>
        onBeforeLinkHook({ req: opts.req as any, providerId: hookProviderId, user: auth.user }),
      )
      try {
        await store.linkIdentity(providerUserId, (opts.identity ?? {}) as any, opts.authId)
      } catch (e) {
        // Lost a race against another link or signup of the same subject.
        if (isUniqueConstraintViolation(e)) {
          throw linkedElsewhereError(providerName)
        }
        throw e
      }
      await onAfterLinkHook({
        req: opts.req as any,
        providerId: hookProviderId,
        user: auth.user,
        oauth,
      })
    },
    unlink: async (providerUserId, opts) => {
      await assertSchemeOwnsAccount(spec, opts.authId)
      const outcome = await store.unlinkIdentity(providerUserId, opts.authId)
      if (outcome === 'not-found') {
        throw contractError('wasp-auth/identity-not-found', `The account holds no such identity in provider name '${providerName}'.`)
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
        const from = await tx.{= authEntityLower =}.findUnique({ where: { id: fromAuthId }, include: { {= userFieldOnAuthEntityName =}: true } })
        const into = await tx.{= authEntityLower =}.findUnique({ where: { id: intoAuthId }, include: { {= userFieldOnAuthEntityName =}: true } })
        if (from?.{= userFieldOnAuthEntityName =} == null || into?.{= userFieldOnAuthEntityName =} == null) {
          throw contractError('wasp-auth/identity-not-found', 'One of the accounts to merge does not exist.')
        }
        // The app goes first, while both rows still exist, so it can
        // re-point whatever `from` owns.
        await mergeUsers({ from: from.{= userFieldOnAuthEntityName =}, into: into.{= userFieldOnAuthEntityName =}, prisma: tx, req: req as any })
        await tx.{= authIdentityEntityLower =}.updateMany({ where: { authId: fromAuthId }, data: { authId: intoAuthId } })
        // Cascades to the old Auth entity and its credentials. A relation
        // the app forgot to re-point fails here and rolls everything back.
        await tx.{= userEntityLower =}.delete({ where: { id: from.{= userFieldOnAuthEntityName =}.id } })
      })
    },
    updateData: (providerUserId, updates) => store.updateData(providerUserId, updates),
    getSecrets: (providerUserId) => store.getSecrets(providerUserId) as any,
    updateSecrets: (providerUserId, updates) => store.updateSecrets(providerUserId, updates),
    delete: (providerUserId) => store.deleteIdentity(providerUserId),
  }
}

// Says nothing about the other account on purpose: the caller proved control
// of the identity, not the right to learn who else holds it.
function linkedElsewhereError(providerName: string): Error {
  return contractError(
    'wasp-auth/identity-linked-elsewhere',
    `The identity in provider name '${providerName}' already belongs to another account.`,
  )
}

/**
 * The account-linking guard: a scheme may only attach identities to (or
 * detach them from) an account that already carries an identity in one of
 * its OWN provider names. Without it, a scheme could attach itself to another
 * scheme's users.
 */
async function assertSchemeOwnsAccount(spec: SchemeRuntimeSpec, authId: string): Promise<void> {
  const ownedIdentity = await prisma.{= authIdentityEntityLower =}.findFirst({
    where: { authId, handlerName: spec.scheme },
    select: { providerName: true },
  })
  if (ownedIdentity === null) {
    throw contractError(
      'wasp-auth/identity-not-found',
      `Auth scheme '${spec.scheme}' has no identity on the account it tried to link.`,
    )
  }
}

// The hook payloads speak `ProviderId` ({ handlerName, providerName, providerUserId }).
function makeHookProviderId(handlerName: string, providerName: string, providerUserId: string): ProviderId {
  return { handlerName, providerName, providerUserId }
}

/**
 * The credentials issuer a scheme signs in through, bound to a target issuer
 * and to the calling scheme. The provider name guard, the identity lookup and
 * the app's login hooks all run here, BEFORE the target issues anything --
 * so no scheme can skip the app's login policy, and the issuer records the
 * calling scheme as `signedInBy` without ever being told a name to record.
 */
function boundTo(spec: SchemeRuntimeSpec, target: AuthHandler, targetIssuerOptions: IssuerOptions | null): CredentialsIssuer {
  if (target.signIn === undefined) {
    throw new Error(`Auth scheme '${spec.scheme}' signs into a scheme whose handler cannot issue credentials.`)
  }
  const signInOnTarget = target.signIn.bind(target)
  const resolveIdentityRef = async (identityRef: AuthIdentityRef) => {
    const providerName = resolveOwnProviderName(spec, identityRef.providerName)
    const identity = await getIdentityStore(spec.scheme, providerName).find(identityRef.providerUserId)
    if (identity === null) {
      throw contractError(
        'wasp-auth/identity-not-found',
        `No identity for the subject in provider name '${providerName}'. Provision it before signing in.`,
      )
    }
    return { providerName, authId: identity.authId }
  }
  return {
    authenticate: (request) => target.authenticate(request),
    signIn: async (identityRef, opts) => {
      const { providerName, authId } = await resolveIdentityRef(identityRef)
      const oauth = resolveOAuthData(spec, providerName, opts?.oauth)
      const fireHooks = opts?.skipHooks !== true
      const hookProviderId = makeHookProviderId(spec.scheme, providerName, identityRef.providerUserId)
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
        { providerName, providerUserId: identityRef.providerUserId },
        { signedInBy: spec.scheme, req: opts?.req, properties: opts?.properties },
      )
      if (fireHooks) {
        await onAfterLoginHook({
          req: opts?.req as any,
          providerId: hookProviderId,
          user: hookUser as any,
          oauth,
        })
      }
      return result
    },
    signOut: (request) => target.signOut?.(request) ?? Promise.resolve({ status: 200, body: { success: true } }),
    signOutEverywhere: async (identityRef) => {
      const { authId } = await resolveIdentityRef(identityRef)
      if (targetIssuerOptions !== null) {
        await signOutEverywhere(targetIssuerOptions, authId)
      }
    },
    createOneTimeCode: (request) => createOneTimeCode(requireWaspIssuer(spec, targetIssuerOptions), request),
    redeemOneTimeCode: (oneTimeCode) => redeemOneTimeCode(requireWaspIssuer(spec, targetIssuerOptions), oneTimeCode),
  }
}

// One-time codes live in a Wasp issuer's credential store. A scheme that
// signs into a hand-written issuer has no such store to put them in.
function requireWaspIssuer(spec: SchemeRuntimeSpec, targetIssuerOptions: IssuerOptions | null): IssuerOptions {
  if (targetIssuerOptions === null) {
    throw new Error(
      `Auth scheme '${spec.scheme}' signs into a scheme that is not a Wasp issuer, so it has no one-time codes.`,
    )
  }
  return targetIssuerOptions
}

{=# isEmailSenderEnabled =}
/**
 * The `email-send` grant: the app's configured email sender, sender identity
 * included. SMTP credentials never reach the handler -- only the send
 * capability does.
 */
const waspEmailFacet: WaspEmail = (() => {
  // Aeson encodes an absent name as null; the contract speaks `name?: string`.
  const configured: { email: string; name?: string | null } | undefined = {=& defaultFromJson =}
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
{=/ isEmailSenderEnabled =}

/**
 * A facet the manifest did not declare. It is still a member of the runtime,
 * so a handler's type never has to claim what its manifest declares; using it
 * fails loudly, naming the scheme and what to declare, instead of surfacing
 * as "cannot read properties of undefined" at the first login.
 */
function undeclaredFacetError(spec: SchemeRuntimeSpec, facet: string, howToDeclare: string): Error {
  return contractError(
    'wasp-auth/undeclared-facet',
    `Auth scheme '${spec.scheme}' used \`runtime.${facet}\`, which its manifest does not declare. ${howToDeclare}`,
  )
}

function undeclaredCredentialsIssuer(spec: SchemeRuntimeSpec): CredentialsIssuer {
  const reject = async (): Promise<never> => {
    throw undeclaredFacetError(
      spec,
      'credentialsIssuer',
      "Declare `credentials` in the manifest ({ transport, store } or { scheme }), or check `runtime.hasCredentialsIssuer` first.",
    )
  }
  return {
    authenticate: reject,
    signIn: reject,
    signOut: reject,
    signOutEverywhere: reject,
    createOneTimeCode: reject,
    redeemOneTimeCode: reject,
  }
}

function undeclaredEmail(spec: SchemeRuntimeSpec): WaspEmail {
  return {
    defaultFrom: undefined,
    send: async () => {
      throw undeclaredFacetError(
        spec,
        'email',
        "Request the 'email-send' grant in the manifest's `uses` (the app must configure an emailSender), or check `runtime.canSendEmail` first.",
      )
    },
  }
}

function makeSchemeRuntime(spec: SchemeRuntimeSpec, credentialsIssuer: CredentialsIssuer | null): WaspServerRuntime<string> {
  {=# isEmailSenderEnabled =}
  const canSendEmail = spec.uses.includes('email-send')
  {=/ isEmailSenderEnabled =}
  {=^ isEmailSenderEnabled =}
  // Validation rejects the grant in an app without an emailSender.
  const canSendEmail = false
  {=/ isEmailSenderEnabled =}
  return {
    scheme: spec.scheme,
    mountPath: `/auth/${spec.scheme}`,
    db: prisma,
    dbProvider: '{= dbProvider =}',
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
    // One store per declared provider name (`identities.email`). The keys are
    // the boundary: an undeclared provider name has no member.
    identities: Object.fromEntries(
      Object.keys(spec.providers).map((providerName) => [providerName, makeIdentitiesFacet(spec, providerName)]),
    ),
    // Every facet is always a member. One the manifest did not declare
    // rejects with a clear error on use; the booleans let a handler branch
    // when availability is the app's choice.
    credentialsIssuer: credentialsIssuer ?? undeclaredCredentialsIssuer(spec),
    hasCredentialsIssuer: credentialsIssuer !== null,
    {=# isEmailSenderEnabled =}
    email: canSendEmail ? waspEmailFacet : undeclaredEmail(spec),
    {=/ isEmailSenderEnabled =}
    {=^ isEmailSenderEnabled =}
    email: undeclaredEmail(spec),
    {=/ isEmailSenderEnabled =}
    canSendEmail,
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

{=# schemes =}

// ---- scheme '{= schemeName =}' ({= handler =}) ----
const spec_{= index =}: SchemeRuntimeSpec = {
  scheme: '{= schemeName =}',
  serverEnvVarNames: {=& serverEnvVarNamesJs =},
  uses: {=& usesJs =},
  providers: {=& providersJs =},
}
{=# inlineCredentials =}
const issuerOptions_{= index =}: IssuerOptions = {
  scheme: '{= schemeName =}',
  transport: '{= transport =}',
  {=# storeModule.isDefined =}
  store: {= storeModule.importIdentifier =},
  {=/ storeModule.isDefined =}
  {=^ storeModule.isDefined =}
  store: '{= storeKind =}',
  {=/ storeModule.isDefined =}
  ttl: '{= ttl =}',
  freshFor: '{= freshFor =}',
  secret: (validatedEnv as Record<string, string | undefined>)['WASP_CREDENTIAL_SECRET'],
  loginPath: `${config.frontendUrl}{= failureRedirectPath =}`,
}
issuerOptionsByScheme['{= schemeName =}'] = issuerOptions_{= index =}
// The private issuer behind this scheme's inline `credentials`.
const issuer_{= index =} = createIssuer(issuerOptions_{= index =})
const credentialsIssuer_{= index =} = boundTo(spec_{= index =}, issuer_{= index =}, issuerOptions_{= index =})
{=/ inlineCredentials =}
{=# credentialsScheme =}
// Signs into the sibling scheme '{= credentialsScheme =}', created above.
const credentialsIssuer_{= index =} = boundTo(spec_{= index =}, handlerOf('{= credentialsScheme =}'), issuerOptionsByScheme['{= credentialsScheme =}'] ?? null)
{=/ credentialsScheme =}
{=^ hasCredentials =}
const credentialsIssuer_{= index =} = null
{=/ hasCredentials =}
{=# isFrameworkIssuer =}
// waspBearer() / waspCookie(): the scheme IS its issuer.
const handlerParts_{= index =} = { handler: issuer_{= index =} }
{=/ isFrameworkIssuer =}
{=^ isFrameworkIssuer =}
{=^ isPackage =}
// An adapter from the app's own code: the same thing a handler package
// exports as `createServerAuthHandler`, so it is instantiated the same way and
// has the same powers (the runtime as an argument, routes of its own).
const createServerAuthHandler_{= index =} = {= handlerModule.importIdentifier =}
{=/ isPackage =}
const handlerParts_{= index =} = await Promise.resolve(
  createServerAuthHandler_{= index =}(
    // The cast narrows the built runtime to what the adapter's type declares
    // (an adapter typed with `ServerAuthAdapterFor` sees only the env vars and
    // facets of its manifest). Sound by construction: the generator wired
    // exactly the manifest's declarations into this runtime.
    makeSchemeRuntime(spec_{= index =}, credentialsIssuer_{= index =}) as unknown as Parameters<typeof createServerAuthHandler_{= index =}>[0],
    // The handler's `server.spec`: its plain data, with every reference
    // to app code set back at the path it was lifted from.
    joinHandlerSpec({=& specJson =}, [
      {=# specReferences =}
      [{=& pathJs =}, {= import.importIdentifier =}],
      {=/ specReferences =}
      // Wasp never reads a handler's spec, so it cannot know its type. The
      // cast is sound by construction: this IS the object the handler's own
      // spec helper built, carried across the compiler.
    ]) as Parameters<typeof createServerAuthHandler_{= index =}>[1],
  ),
)
{=/ isFrameworkIssuer =}
registered['{= schemeName =}'] = handlerParts_{= index =}
{=/ schemes =}

// PRIVATE API
/**
 * The app's auth schemes, keyed by name. Everything else in Wasp depends on
 * the `AuthHandler` interface rather than on concrete implementations.
 */
export const authSchemes: { readonly [Name in AuthSchemeName]: AuthHandler } = {
  {=# schemes =}
  '{= schemeName =}': handlerOf('{= schemeName =}'),
  {=/ schemes =}
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
  {=# schemes =}
  {=^ isFrameworkIssuer =}
  '{= schemeName =}': handlerParts_{= index =}.routeHandler,
  {=/ isFrameworkIssuer =}
  {=/ schemes =}
}

/**
 * Each manifest in `main.wasp.ts` made compile-time claims about its handler
 * (its capabilities), and code was generated from them. Checking the claims
 * against the handler objects at boot turns a wrong manifest into a loud
 * startup failure instead of a subtly broken app.
 */
function assertHandlersMatchManifests(): void {
  const manifests: Array<{ scheme: AuthSchemeName; capabilities: string[] }> = [
    {=# schemes =}
    { scheme: '{= schemeName =}', capabilities: {=& capabilitiesJs =} },
    {=/ schemes =}
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
