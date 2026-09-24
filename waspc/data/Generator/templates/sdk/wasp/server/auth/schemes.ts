{{={= =}=}}
import type { AccountPrincipal, AuthenticateResult, CredentialHandler, ResolvedIdentity, CredentialsIssuer, IdentityPrincipal, IdentityStore, AuthIdentityRef, OAuthLoginData, SignInOpts, SignInResult, WaspEmail, WaspServerRuntime } from './handler/types.js'
import type { OAuthData } from './hooks.js'
import type { AuthSchemeName } from '../../auth/scheme.js'
import { joinHandlerSpec } from '../../auth/handlerSpec.js'
import { authenticateAccount, computeSchemeUserFields, provisionAuthUser } from './session.js'
import { getIdentityStore } from './identityStore.js'
import { createIssuer, deleteCredential, keepsCredentialsInStore, signOutEverywhere, type IssuerHandler, type IssuerOptions } from './issuer.js'
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
import { getCurrentRequest } from '../requestContext.js'
import { toWebRequest } from './http.js'
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
      }, providerName, { oauth: resolveOAuthData(spec, providerName, opts?.oauth) }),
    create: async (providerUserId, opts) => {
      const { identity, getUserFields } = opts ?? {}
      const oauth = resolveOAuthData(spec, providerName, opts?.oauth)
      // The app's signup veto fires FIRST -- at this Wasp-owned choke point no
      // handler can forget it -- and only then do any user-supplied field
      // getters run (that ordering is why `getUserFields` is a lazy callback).
      if (opts?.skipHooks !== true) {
        await fireVetoableHook(() =>
          onBeforeSignupHook({
            req: getCurrentRequest() as any,
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
          req: getCurrentRequest() as any,
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
        onBeforeLinkHook({ req: getCurrentRequest() as any, providerId: hookProviderId, user: auth.user }),
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
        req: getCurrentRequest() as any,
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
    merge: async ({ fromAuthId, intoAuthId }) => {
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
      // The handler proved `from` moments ago; Wasp proves `into` itself: the
      // request must carry a FRESH credential of the surviving account.
      const req = getCurrentRequest()
      await assertFreshCredentialOf(spec, intoAuthId, req)
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
    reportLogin: async (providerUserId, opts) => {
      const oauth = resolveOAuthData(spec, providerName, opts?.oauth)
      const identity = await store.find(providerUserId)
      if (identity === null) {
        throw contractError('wasp-auth/identity-not-found', `No identity '${providerUserId}' in provider '${providerName}' to report a login for.`)
      }
      const auth = await findAuthWithUserBy({ id: identity.authId })
      if (auth === null) {
        throw contractError('wasp-auth/identity-not-found', 'The identity resolves to an auth entity with no user.')
      }
      const hookProviderId = makeHookProviderId(spec.scheme, providerName, providerUserId)
      await fireVetoableHook(() =>
        onBeforeLoginHook({ req: getCurrentRequest() as any, providerId: hookProviderId, user: auth.user }),
      )
      await onAfterLoginHook({ req: getCurrentRequest() as any, providerId: hookProviderId, user: auth.user, oauth })
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
/**
 * A merge is a sensitive operation: the surviving account must be the one
 * making the request, with a credential issued within `freshFor`. A stored
 * weeks-old credential does not do; neither does a request from nobody.
 */
async function assertFreshCredentialOf(
  spec: SchemeRuntimeSpec,
  authId: string,
  req: ReturnType<typeof getCurrentRequest>,
): Promise<void> {
  const notFresh = (why: string) =>
    contractError('wasp-auth/credential-not-fresh', `Accounts can only be merged by the surviving account, with a fresh credential: ${why}`)
  if (req === undefined) {
    throw notFresh('the merge was not requested inside a request.')
  }
  const account = await authenticateAccount(spec.scheme, toWebRequest(req))
  if (account === null) {
    throw notFresh('the request carries no valid credential.')
  }
  if (!account.isCredentialFresh) {
    throw notFresh(`the credential was issued longer than '${spec.scheme}' considers fresh ago. Log in again.`)
  }
  if (account.authId !== authId) {
    throw notFresh('the request is from another account than the surviving one.')
  }
}

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
 * so no scheme can skip the app's login policy, and the issuer receives the
 * calling scheme as the identity's `handlerName`, never a name of the
 * handler's choosing.
 */
/** What the imperative `signIn` needs from a scheme: issue for a known identity, hooks included. */
type IssueSignIn = (
  identity: { providerName: string; providerUserId: string; authId: string },
  opts: SignInOpts | undefined,
  oauth: OAuthData | undefined,
) => Promise<SignInResult>

function boundTo(spec: SchemeRuntimeSpec, target: CredentialHandler): { facet: CredentialsIssuer; issueSignIn: IssueSignIn } {
  if (target.signIn === undefined) {
    throw new Error(`Auth scheme '${spec.scheme}' signs into a scheme whose handler cannot issue credentials.`)
  }
  const signInOnTarget = target.signIn.bind(target)
  const resolvedIdentityOf = (providerName: string, providerUserId: string, authId: string): ResolvedIdentity => ({
    handlerName: spec.scheme,
    providerName,
    providerUserId,
    authId,
  })
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
  // The app's login hooks fire around the target's sign-in, here, so no
  // scheme can skip the app's login policy.
  const issueSignIn: IssueSignIn = async ({ providerName, providerUserId, authId }, opts, oauth) => {
    const fireHooks = opts?.skipHooks !== true
    const hookProviderId = makeHookProviderId(spec.scheme, providerName, providerUserId)
    let hookUser: unknown = undefined
    if (fireHooks) {
      const auth = await findAuthWithUserBy({ id: authId })
      if (auth === null) {
        throw contractError('wasp-auth/identity-not-found', 'The subject resolves to an auth entity with no user.')
      }
      hookUser = auth.user
      await fireVetoableHook(() =>
        onBeforeLoginHook({ req: getCurrentRequest() as any, providerId: hookProviderId, user: auth.user }),
      )
    }
    const result = await signInOnTarget(resolvedIdentityOf(providerName, providerUserId, authId), opts?.properties)
    if (fireHooks) {
      await onAfterLoginHook({
        req: getCurrentRequest() as any,
        providerId: hookProviderId,
        user: hookUser as any,
        oauth,
      })
    }
    return result
  }
  const facet: CredentialsIssuer = {
    signIn: async (identityRef, opts) => {
      const { providerName, authId } = await resolveIdentityRef(identityRef)
      const oauth = resolveOAuthData(spec, providerName, opts?.oauth)
      return issueSignIn({ providerName, providerUserId: identityRef.providerUserId, authId }, opts, oauth)
    },
    signOut: (request) => target.signOut?.(request) ?? Promise.resolve(Response.json({ success: true })),
    signOutEverywhere: async (identityRef) => {
      const { providerName, authId } = await resolveIdentityRef(identityRef)
      await target.signOutEverywhere?.(resolvedIdentityOf(providerName, identityRef.providerUserId, authId))
    },
  }
  return { facet, issueSignIn }
}

/** The facet for signing this scheme's identities into any issuing scheme, built on first use. */
const issuerFacetsByPair = new Map<string, CredentialsIssuer>()

function issuerFacetFor(spec: SchemeRuntimeSpec, scheme: string): CredentialsIssuer {
  const pair = `${spec.scheme} -> ${scheme}`
  let facet = issuerFacetsByPair.get(pair)
  if (facet === undefined) {
    if (!authSchemeNames.includes(scheme as AuthSchemeName)) {
      throw new Error(`Auth scheme '${spec.scheme}' asked to sign into '${scheme}', which is not one of the app's auth schemes.`)
    }
    facet = boundTo(spec, signInTargetOf(scheme as AuthSchemeName)).facet
    issuerFacetsByPair.set(pair, facet)
  }
  return facet
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
    signIn: reject,
    signOut: reject,
    signOutEverywhere: reject,
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
    authenticate: (request) => authenticateAccount(spec.scheme, request),
    // One store per declared provider name (`identities.email`). The keys are
    // the boundary: an undeclared provider name has no member.
    identities: Object.fromEntries(
      Object.keys(spec.providers).map((providerName) => [providerName, makeIdentitiesFacet(spec, providerName)]),
    ),
    // Every facet is always a member. One the manifest did not declare
    // rejects with a clear error on use; the booleans let a handler branch
    // when availability is the app's choice.
    credentialsIssuer: credentialsIssuer ?? undeclaredCredentialsIssuer(spec),
    credentialsIssuerFor: (scheme) => issuerFacetFor(spec, scheme),
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

/** The sign-in of every scheme that can issue credentials, for the imperative API. */
const signInByScheme: Partial<Record<AuthSchemeName, IssueSignIn>> = {}

type RouteHandler = (request: Request) => Response | Promise<Response>

/** What Wasp holds per scheme: who recognises its credential, and its routes. */
type SchemeParts = {
  /** Wasp's own issuer for the scheme's inline `credentials`, when it has them: an `CredentialHandler` with no login of its own. */
  issuer: IssuerHandler | null
  /** The sibling scheme this one signs into (`credentials: { scheme }`), when it does. */
  credentialsScheme: AuthSchemeName | null
  /** The scheme's own credential handler; null when Wasp's credential is all it has. */
  credentialHandler: CredentialHandler | null
  routeHandler?: RouteHandler
}

// Filled in dependency order below.
const registered: Partial<Record<AuthSchemeName, SchemeParts>> = {}

function partsOf(name: AuthSchemeName): SchemeParts {
  const parts = registered[name]
  if (parts === undefined) {
    throw new Error(`Auth scheme '${name}' is not built yet; the generator ordered the schemes wrong.`)
  }
  return parts
}

type ChainLink = { scheme: AuthSchemeName; credentialHandler: CredentialHandler }

/**
 * Every handler that may recognise a scheme's requests, in the order Wasp
 * asks them: the credential the scheme signs into first (a sibling's chain,
 * then Wasp's own issuer), the scheme's own credential handler last.
 */
function chainOf(name: AuthSchemeName): ChainLink[] {
  const parts = partsOf(name)
  const signedInto = parts.credentialsScheme === null ? [] : chainOf(parts.credentialsScheme)
  const own = [parts.issuer, parts.credentialHandler].flatMap((credentialHandler) =>
    credentialHandler === null ? [] : [{ scheme: name, credentialHandler }],
  )
  return [...signedInto, ...own]
}

/** What a scheme signs into: the first handler on its chain that can issue. */
function signInTargetOf(name: AuthSchemeName): CredentialHandler {
  const issuing = chainOf(name).find(({ credentialHandler }) => credentialHandler.signIn !== undefined)
  if (issuing === undefined) {
    throw new Error(`Auth scheme '${name}' has nothing to sign into.`)
  }
  return issuing.credentialHandler
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
  slidingRenewal: {=# slidingRenewal =}'{= slidingRenewal =}'{=/ slidingRenewal =}{=^ slidingRenewal =}null{=/ slidingRenewal =},
  secret: (validatedEnv as Record<string, string | undefined>)['WASP_CREDENTIAL_SECRET'],
  loginPath: `${config.frontendUrl}{= failureRedirectPath =}`,
}
issuerOptionsByScheme['{= schemeName =}'] = issuerOptions_{= index =}
// The private issuer behind this scheme's inline `credentials`.
const issuer_{= index =} = createIssuer(issuerOptions_{= index =})
const bound_{= index =} = boundTo(spec_{= index =}, issuer_{= index =})
const credentialsIssuer_{= index =} = bound_{= index =}.facet
signInByScheme['{= schemeName =}'] = bound_{= index =}.issueSignIn
{=/ inlineCredentials =}
{=# credentialsScheme =}
// Signs into the sibling scheme '{= credentialsScheme =}', created above.
const bound_{= index =} = boundTo(spec_{= index =}, signInTargetOf('{= credentialsScheme =}'))
const credentialsIssuer_{= index =} = bound_{= index =}.facet
signInByScheme['{= schemeName =}'] = bound_{= index =}.issueSignIn
{=/ credentialsScheme =}
{=^ hasCredentials =}
const credentialsIssuer_{= index =} = null
{=/ hasCredentials =}
{=# isFrameworkIssuer =}
// waspBearer() / waspCookie(): the scheme IS its issuer, and nothing else.
registered['{= schemeName =}'] = { issuer: issuer_{= index =}, credentialsScheme: null, credentialHandler: null }
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
registered['{= schemeName =}'] = {
  {=# inlineCredentials =}
  issuer: issuer_{= index =},
  {=/ inlineCredentials =}
  {=^ inlineCredentials =}
  issuer: null,
  {=/ inlineCredentials =}
  {=# credentialsScheme =}
  credentialsScheme: '{= credentialsScheme =}',
  {=/ credentialsScheme =}
  {=^ credentialsScheme =}
  credentialsScheme: null,
  {=/ credentialsScheme =}
  // Absent when Wasp issues the credential and the handler has none of its own.
  credentialHandler: handlerParts_{= index =}.credentialHandler ?? null,
  routeHandler: handlerParts_{= index =}.routeHandler,
}
{=^ hasCredentials =}
// A handler that issues its own credential can still be signed into by app
// code (the imperative `signIn`), through the same guarded path as any issuer.
if (handlerParts_{= index =}.credentialHandler?.signIn !== undefined) {
  signInByScheme['{= schemeName =}'] = boundTo(spec_{= index =}, handlerParts_{= index =}.credentialHandler).issueSignIn
}
{=/ hasCredentials =}
{=/ isFrameworkIssuer =}
{=/ schemes =}

// PRIVATE API
export const authSchemeNames: readonly AuthSchemeName[] = [
  {=# schemes =}
  '{= schemeName =}',
  {=/ schemes =}
]

// Every scheme must be recognisable: through the credential it signs into,
// or through a handler of its own. The types say the same for a typed
// adapter; this covers the hand-written ones.
for (const name of authSchemeNames) {
  if (chainOf(name).length === 0) {
    throw new Error(
      `Auth scheme '${name}' declares no \`credentials\` and its adapter returned no handler, so nothing could recognise its requests.`,
    )
  }
}

// PRIVATE API
/** The scheme's own credential handler; null when Wasp's credential is all it has. */
export function credentialHandlerOf(name: AuthSchemeName): CredentialHandler | null {
  return partsOf(name).credentialHandler
}

// PRIVATE API
/** Whose request this is, as one scheme sees it: an account (Wasp's own credential) or a handler's identity. */
export type SchemeAuthentication =
  | { kind: 'account'; scheme: AuthSchemeName; account: AccountPrincipal; loginScheme: string }
  | { kind: 'identity'; scheme: AuthSchemeName; principal: IdentityPrincipal }

type ChainAuthentication = ChainLink & { result: Extract<AuthenticateResult, { status: 'authenticated' }> }

/** The first handler on the scheme's chain that recognises the request. A handler that throws is logged and treated as `unauthenticated`. */
async function authenticateAlongChain(scheme: AuthSchemeName, request: Request): Promise<ChainAuthentication | null> {
  for (const link of chainOf(scheme)) {
    const result = await Promise.resolve()
      .then(() => link.credentialHandler.authenticate(request))
      .catch((error) => {
        console.error(`Auth scheme '${link.scheme}' threw while authenticating:`, error)
        return { status: 'unauthenticated' } as const
      })
    if (result.status === 'authenticated') {
      return { ...link, result }
    }
  }
  return null
}

// PRIVATE API
/**
 * Authenticates a request for one scheme: Wasp's own credential first (an
 * account, no lookup needed), then the credential handler's `authenticate`
 * (an identity Wasp resolves, provisioning on first sight). A handler that
 * throws instead of answering `unauthenticated` is logged and treated as
 * `unauthenticated`: how it rejects a bad credential is its own business.
 */
export async function authenticateScheme(scheme: AuthSchemeName, request: Request): Promise<SchemeAuthentication | null> {
  const authentication = await authenticateAlongChain(scheme, request)
  return authentication === null ? null : toSchemeAuthentication(authentication)
}

function toSchemeAuthentication({ scheme: owner, result }: ChainAuthentication): SchemeAuthentication {
  return 'account' in result
    ? { kind: 'account', scheme: owner, account: result.account, loginScheme: result.loginScheme ?? owner }
    : { kind: 'identity', scheme: owner, principal: result.principal }
}

// PRIVATE API
/** Ends the credential the request carries, through the handler that recognised it. */
export async function signOutScheme(scheme: AuthSchemeName, request: Request): Promise<Response> {
  const authentication = await authenticateAlongChain(scheme, request)
  return (await authentication?.credentialHandler.signOut?.(request)) ?? Response.json({ success: true })
}

/** The scheme's own handler answers first, the credential it signs into after: the handler nearest the person knows where its login page is. */
async function firstAnswerAlongChain(
  scheme: AuthSchemeName,
  ask: (credentialHandler: CredentialHandler) => Promise<Response> | undefined,
): Promise<Response | undefined> {
  for (const { credentialHandler } of chainOf(scheme).reverse()) {
    const answer = await ask(credentialHandler)
    if (answer !== undefined) {
      return answer
    }
  }
  return undefined
}

// PRIVATE API
/** "Log in first", the way the scheme says it: a redirect for a cookie scheme, else 401. */
export async function challengeScheme(scheme: AuthSchemeName, request: Request): Promise<Response> {
  return (
    (await firstAnswerAlongChain(scheme, (credentialHandler) => credentialHandler.challenge?.(request))) ??
    Response.json({ message: 'Invalid credentials' }, { status: 401 })
  )
}

// PRIVATE API
/** "You may not", the way the scheme says it. Default: 403. */
export async function forbidScheme(scheme: AuthSchemeName, request: Request): Promise<Response> {
  return (
    (await firstAnswerAlongChain(scheme, (credentialHandler) => credentialHandler.forbid?.(request))) ??
    Response.json({ message: 'Forbidden' }, { status: 403 })
  )
}

// PRIVATE API
/**
 * Issues a credential of `scheme` for an identity the imperative API looked
 * up. Throws `wasp-auth/undeclared-facet` when the scheme cannot issue one.
 */
export function issueSignInFor(
  scheme: AuthSchemeName,
  identity: { providerName: string; providerUserId: string; authId: string },
  opts: SignInOpts | undefined,
): Promise<SignInResult> {
  const issueSignIn = signInByScheme[scheme]
  if (issueSignIn === undefined) {
    throw contractError(
      'wasp-auth/undeclared-facet',
      `Auth scheme '${scheme}' declares no \`credentials\` and its handler cannot issue one, so Wasp cannot sign a user into it.`,
    )
  }
  return issueSignIn(identity, opts, undefined)
}

/** Wasp's own issuer on a scheme's chain, following `credentials: { scheme }`; null when the chain has none. */
function waspIssuerOf(name: AuthSchemeName): IssuerHandler | null {
  const parts = partsOf(name)
  return parts.issuer ?? (parts.credentialsScheme === null ? null : waspIssuerOf(parts.credentialsScheme))
}

// PRIVATE API
/**
 * Reissues the Wasp credential the request carries under `scheme`: the
 * answer to send, or null when the request carries no Wasp credential of
 * that scheme's chain (anonymous, or a handler-owned credential).
 */
export async function refreshSignInFor(scheme: AuthSchemeName, request: Request): Promise<Response | null> {
  const issuer = waspIssuerOf(scheme)
  return issuer === null ? null : issuer.refresh(request)
}

// PRIVATE API
/** Whether the request carries a credential Wasp issued on `scheme`'s chain, rather than a handler's own. */
export async function carriesWaspCredential(scheme: AuthSchemeName, request: Request): Promise<boolean> {
  const issuer = waspIssuerOf(scheme)
  if (issuer === null) {
    return false
  }
  const result = await issuer.authenticate(request)
  return result.status === 'authenticated'
}

// PRIVATE API
/**
 * Ends every credential of ONE scheme for the account, eagerly: the scheme's
 * own session rows when Wasp issues them, or the handler's sessions through
 * its `signOutEverywhere`. Throws when the scheme cannot do that.
 */
export async function signOutEverywhereInScheme(scheme: AuthSchemeName, authId: string): Promise<void> {
  const options = issuerOptionsByScheme[scheme]
  if (options !== undefined) {
    if (!keepsCredentialsInStore(options)) {
      throw new Error(
        `Auth scheme '${scheme}' issues signed tokens, which have no rows to end per scheme. signOutEverywhere(user) without a scheme ends them all.`,
      )
    }
    await signOutEverywhere(options, authId)
    return
  }
  const credentialHandler = credentialHandlerOf(scheme)
  if (credentialHandler === null) {
    throw new Error(`Auth scheme '${scheme}' has no credentials of its own; it signs into another scheme.`)
  }
  if (credentialHandler.signOutEverywhere === undefined) {
    throw new Error(`Auth scheme '${scheme}' cannot end its credentials: its handler implements no signOutEverywhere.`)
  }
  const identities = await prisma.{= authIdentityEntityLower =}.findMany({
    where: { authId, handlerName: scheme },
    select: { handlerName: true, providerName: true, providerUserId: true, authId: true },
  })
  for (const identity of identities) {
    await credentialHandler.signOutEverywhere(identity)
  }
}

// PRIVATE API
/**
 * Ends one Wasp-issued credential by id, in every scheme that keeps its
 * credentials in a store. Resolves to whether any scheme does; a signed
 * token has no row to delete.
 */
export async function signOutCredentialById(credentialId: string): Promise<boolean> {
  let acted = false
  for (const options of Object.values(issuerOptionsByScheme)) {
    if (options !== undefined && keepsCredentialsInStore(options)) {
      await deleteCredential(options, credentialId)
      acted = true
    }
  }
  return acted
}

// PRIVATE API
/**
 * The routes schemes brought with them, keyed by scheme name: standard
 * `Request` in, `Response` out. The server mounts each at `/auth/<scheme>`.
 */
export const authSchemeRouteHandlers: Partial<Record<AuthSchemeName, (request: Request) => Response | Promise<Response>>> = {
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
    const parts = partsOf(manifest.scheme)
    if (parts.issuer === null && parts.credentialHandler === null && parts.credentialsScheme === null) {
      errors.push(`scheme '${manifest.scheme}' has nothing that could recognise a credential: no handler, no credentials`)
    }
    const signInTarget = parts.issuer ?? parts.credentialHandler
    if (manifest.capabilities.includes('sign-in') && typeof signInTarget?.signIn !== 'function') {
      errors.push(
        `the manifest of scheme '${manifest.scheme}' declares the 'sign-in' capability, but nothing in it can sign in`,
      )
    }
  }
  if (errors.length > 0) {
    throw new Error('Auth handlers do not match their manifests:\n' + errors.map((error) => `  - ${error}`).join('\n'))
  }
}

assertHandlersMatchManifests()
