{{={= =}=}}
import type {
  {= userEntityName =},
  {= authEntityName =},
  {= authIdentityEntityName =},
} from '../entities/index.js'
import { parseProviderData } from './providerData.js'
import type { AuthSchemeName } from './scheme.js'

// PUBLIC API
export function getFirstProviderUserId(user?: UserEntityWithAuth): string | null {
  if (!user || !user.auth || !user.auth.identities || user.auth.identities.length === 0) {
    return null;
  }

  return user.auth.identities[0]?.providerUserId ?? null;
}

// PUBLIC API
/**
 * One identity of the user, as the auth provider that owns it recorded it:
 * the auth handler it belongs to (`wasp`, `clerk`), the provider name it
 * lives under (`email`, `default`), the provider's own id
 * for the subject, the claims the provider verified, and its non-secret
 * working data. Provider packages ship typed views over this (Wasp's own
 * auth's `getEmail`/`getUsername`).
 */
export type AuthUserIdentity = {
  handlerName: string
  providerName: string
  providerUserId: string
  claims: Record<string, unknown>
  data: Record<string, unknown>
}

// PUBLIC API
export type AuthUser = AuthUserData & {
  getFirstProviderUserId: () => string | null,
}

// PRIVATE API (used in SDK and server)
/*
 * Ideally, we'd do something like this:
 * ```
 * export type AuthUserData = ReturnType<typeof createAuthUserData>
 * ```
 * to get the benefits of the createAuthUser and the AuthUserData type being in sync.
 *
 * But since we are not using strict mode, the inferred return type of createAuthUser
 * is not correct. So we have to define the AuthUserData type manually.
 *
 * TODO: Change this once/if we switch to strict mode. https://github.com/wasp-lang/wasp/issues/1938
 */
export type AuthUserData = Omit<CompleteUserEntityWithAuth, '{= authFieldOnUserEntityName =}'> & {
  /**
   * The scheme that authenticated the current request: the one whose
   * credential the request carried ('session', 'clerk', ...). Exactly one
   * scheme answers a request, so this is a single compile-checked literal.
   */
  sessionScheme: AuthSchemeName,
  /**
   * The scheme that verified the login this request's credential descends
   * from. Equal to `sessionScheme` unless that scheme is a credential issuer
   * another scheme signed into (Wasp's own auth signing into a cookie
   * scheme, say). This is what `authRequired: ["wasp"]` checks against.
   */
  signedInBy: AuthSchemeName,
  /**
   * Every identity of this user, across all providers and provider names.
   */
  identities: AuthUserIdentity[],
}

// PRIVATE API
export type CompleteUserEntityWithAuth =
  MakeUserEntityWithAuth<CompleteAuthEntityWithIdentities>

// PRIVATE API
// The identity rows as the (secrets-omitting) Prisma client returns them.
export type CompleteAuthEntityWithIdentities =
  MakeAuthEntityWithIdentities<Omit<{= authIdentityEntityName =}, 'providerSecrets'>>

// PRIVATE API
/**
 * User entity with all of the auth related data that's needed for the user facing
 * helper functions like `getFirstProviderUserId`.
 */
export type UserEntityWithAuth = MakeUserEntityWithAuth<
  MakeAuthEntityWithIdentities<
    // It's constructed like the Complete* types, but only with the fields needed
    // for the user facing functions.
    Pick<{= authIdentityEntityName =}, 'handlerName' | 'providerName' | 'providerUserId'>
  >
>

type MakeUserEntityWithAuth<AuthType> = {= userEntityName =} & {
  {= authFieldOnUserEntityName =}: AuthType | null
}

type MakeAuthEntityWithIdentities<IdentityType> = {= authEntityName =} & {
  {= identitiesFieldOnAuthEntityName =}: IdentityType[]
}

// PRIVATE API (used in SDK and server)
export function makeAuthUserIfPossible(user: null): null
export function makeAuthUserIfPossible(user: AuthUserData): AuthUser
export function makeAuthUserIfPossible(user: AuthUserData | null): AuthUser | null
export function makeAuthUserIfPossible(
  user: AuthUserData | null,
): AuthUser | null {
  return user ? makeAuthUser(user) : null
}

function makeAuthUser(data: AuthUserData): AuthUser {
  return {
    ...data,
    getFirstProviderUserId: () => data.identities[0]?.providerUserId ?? null,
  };
}

// PRIVATE API
export function createAuthUserData(
  user: CompleteUserEntityWithAuth,
  sessionScheme: string,
  signedInBy: string,
): AuthUserData {
  const { {= authFieldOnUserEntityName =}, ...rest } = user
  if (!{= authFieldOnUserEntityName =}) {
    throw new Error(`🐝 Error: trying to create a user without auth data.
This should never happen, but it did which means there is a bug in the code.`)
  }
  const identities = {= authFieldOnUserEntityName =}.{= identitiesFieldOnAuthEntityName =}.map((identity) => ({
    handlerName: identity.handlerName,
    providerName: identity.providerName,
    providerUserId: identity.providerUserId,
    claims: parseProviderData(identity.providerClaims),
    data: parseProviderData(identity.providerData),
  }))
  return {
    ...rest,
    sessionScheme: sessionScheme as AuthSchemeName,
    signedInBy: signedInBy as AuthSchemeName,
    identities,
  }
}
