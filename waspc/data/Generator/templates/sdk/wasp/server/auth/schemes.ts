{{={= =}=}}
import type { AuthHandler, Credentials, ProviderIdentities, RuntimeGrantName, Subject, WaspEmail, WaspServerRuntime } from './handler/types.js'
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
{=# isEmailSenderEnabled =}
import { emailSender } from '../email/index.js'
{=/ isEmailSenderEnabled =}
{=# schemes =}
{=# isPackage =}
{=^ isFrameworkIssuer =}
import { createServerAdapter as createServerAdapter_{= index =} } from '{= serverPackage =}'
{=/ isFrameworkIssuer =}
{=/ isPackage =}
{=^ isPackage =}
{=& handlerModule.importStatement =}
{=/ isPackage =}
{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
{=/ setupFn.isDefined =}
{=# extensions =}
{=& import.importStatement =}
{=/ extensions =}
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
      return { authId: created.{= authFieldOnUserEntityName =}!.id }
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
  const ownedIdentity = await prisma.{= authIdentityEntityLower =}.findFirst({
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

function makeSchemeRuntime(spec: SchemeRuntimeSpec, credentials: Credentials | null): WaspServerRuntime<never, false> {
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
    identities: makeIdentitiesFacet(spec, spec.scheme),
    ...(credentials !== null ? { credentials } : {}),
    // Granted facets: wired only when the manifest requested them, so an
    // undeclared access fails loudly at first use rather than working by
    // accident.
    {=# isEmailSenderEnabled =}
    ...(spec.uses.includes('email-send') ? { email: waspEmailFacet } : {}),
    {=/ isEmailSenderEnabled =}
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
const schemeRuntimes: Partial<Record<AuthSchemeName, WaspServerRuntime<never, false>>> = {}

function handlerOf(name: AuthSchemeName): AuthHandler {
  const adapter = registered[name]
  if (adapter === undefined) {
    throw new Error(`Auth scheme '${name}' is not built yet; the generator ordered the schemes wrong.`)
  }
  return adapter.handler
}

{=# schemes =}

// ---- scheme '{= schemeName =}' ({= handler =}) ----
const spec_{= index =}: SchemeRuntimeSpec = {
  scheme: '{= schemeName =}',
  serverEnvVarNames: {=& serverEnvVarNamesJs =},
  uses: {=& usesJs =},
  identityNamespaces: {=& identityNamespacesJs =},
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
  secret: (validatedEnv as Record<string, string | undefined>)['WASP_CREDENTIAL_SECRET'],
  loginPath: `${config.frontendUrl}{= failureRedirectPath =}`,
}
issuerOptionsByScheme['{= schemeName =}'] = issuerOptions_{= index =}
// The private issuer behind this scheme's inline `credentials`.
const issuer_{= index =} = createIssuer(issuerOptions_{= index =})
const credentials_{= index =} = boundTo(spec_{= index =}, issuer_{= index =}, issuerOptions_{= index =})
{=/ inlineCredentials =}
{=# credentialsScheme =}
// Signs into the sibling scheme '{= credentialsScheme =}', created above.
const credentials_{= index =} = boundTo(spec_{= index =}, handlerOf('{= credentialsScheme =}'), issuerOptionsByScheme['{= credentialsScheme =}'] ?? null)
{=/ credentialsScheme =}
{=^ hasCredentials =}
const credentials_{= index =} = null
{=/ hasCredentials =}
{=# isFrameworkIssuer =}
// waspBearer() / waspCookie(): the scheme IS its issuer.
const adapter_{= index =} = { handler: issuer_{= index =} }
{=/ isFrameworkIssuer =}
{=^ isFrameworkIssuer =}
{=# isPackage =}
const adapter_{= index =} = await Promise.resolve(
  createServerAdapter_{= index =}(
    // The cast narrows the built runtime to the grants the factory's type
    // declares; the generator wired exactly the manifest's `uses`, and the
    // boot assert keeps manifest and handler honest.
    makeSchemeRuntime(spec_{= index =}, credentials_{= index =}) as Parameters<typeof createServerAdapter_{= index =}>[0],
    {=& optionsJson =},
    {
      // The user's setup function for the handler's underlying library; the
      // handler calls it with its integration config and uses the result.
      setupFn: {=# setupFn.isDefined =}{= setupFn.importIdentifier =}{=/ setupFn.isDefined =}{=^ setupFn.isDefined =}undefined{=/ setupFn.isDefined =},
      // Every other user function the manifest referenced, under the name
      // the handler expects.
      {=# extensions =}
      '{= name =}': {= import.importIdentifier =},
      {=/ extensions =}
    },
  ),
)
{=/ isPackage =}
{=^ isPackage =}
// A hand-written handler from the app's own code. Its runtime is reachable
// through `getSchemeRuntime('{= schemeName =}')`.
const adapter_{= index =} = { handler: {= handlerModule.importIdentifier =} }
schemeRuntimes['{= schemeName =}'] = makeSchemeRuntime(spec_{= index =}, credentials_{= index =})
{=/ isPackage =}
{=/ isFrameworkIssuer =}
registered['{= schemeName =}'] = adapter_{= index =}
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
 * The runtime of a hand-written scheme, for app code that implements a
 * handler in `src/` and needs the identity store or credentials facet bound
 * to its own scheme. The type parameters state what the scheme's manifest
 * declared: its `uses` grants, and whether it has `credentials` (the facet
 * is absent at runtime otherwise).
 */
export function getSchemeRuntime<
  Grants extends RuntimeGrantName = never,
  HasCredentials extends boolean = false,
>(name: AuthSchemeName): WaspServerRuntime<Grants, HasCredentials> {
  const runtime = schemeRuntimes[name]
  if (runtime === undefined) {
    throw new Error(`Auth scheme '${name}' is a handler package; its runtime is not exposed to app code.`)
  }
  return runtime as unknown as WaspServerRuntime<Grants, HasCredentials>
}

// PRIVATE API
/**
 * Node handlers for the routes schemes brought with them, keyed by scheme
 * name. The server mounts each at `/auth/<scheme>`.
 */
export const authSchemeRouteHandlers: Partial<Record<AuthSchemeName, (req: import('node:http').IncomingMessage, res: import('node:http').ServerResponse) => void | Promise<void>>> = {
  {=# schemes =}
  {=# isPackage =}
  {=^ isFrameworkIssuer =}
  '{= schemeName =}': adapter_{= index =}.routeHandler,
  {=/ isFrameworkIssuer =}
  {=/ isPackage =}
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
