import {
  type User,
  type Auth,
  type AuthIdentity,
} from '../entities/index.js'
import { parseProviderData } from './providerData.js'
import { type AuthProviderId } from './provider.js'

// PUBLIC API
export function getFirstProviderUserId(user?: UserEntityWithAuth): string | null {
  if (!user || !user.auth || !user.auth.identities || user.auth.identities.length === 0) {
    return null;
  }

  return user.auth.identities[0].providerUserId ?? null;
}

// PUBLIC API
/**
 * One identity of the user, as the auth provider that owns it recorded it:
 * the namespace it lives in (`wasp:email`, `clerk`), the provider's own id
 * for the subject, the claims the provider verified, and its non-secret
 * working data. Provider packages ship typed views over this (Wasp's own
 * auth's `getEmail`/`getUsername`).
 */
export type AuthUserIdentity = {
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
export type AuthUserData = Omit<CompleteUserEntityWithAuth, 'auth'> & {
  /**
   * Id of the auth provider that minted the current session -- i.e. how this
   * user logged in this time ('wasp', 'clerk', ...). A session is always
   * minted by exactly one provider, so this is a single compile-checked
   * literal, pinned when the session was created and never re-derived.
   */
  sessionProviderId: AuthProviderId,
  /**
   * Every identity of this user, across all providers and namespaces.
   */
  identities: AuthUserIdentity[],
}

// PRIVATE API
export type CompleteUserEntityWithAuth =
  MakeUserEntityWithAuth<CompleteAuthEntityWithIdentities>

// PRIVATE API
// The identity rows as the (secrets-omitting) Prisma client returns them.
export type CompleteAuthEntityWithIdentities =
  MakeAuthEntityWithIdentities<Omit<AuthIdentity, 'providerSecrets'>>

// PRIVATE API
/**
 * User entity with all of the auth related data that's needed for the user facing
 * helper functions like `getFirstProviderUserId`.
 */
export type UserEntityWithAuth = MakeUserEntityWithAuth<
  MakeAuthEntityWithIdentities<
    // It's constructed like the Complete* types, but only with the fields needed
    // for the user facing functions.
    Pick<AuthIdentity, 'providerName' | 'providerUserId'>
  >
>

type MakeUserEntityWithAuth<AuthType> = User & {
  auth: AuthType | null
}

type MakeAuthEntityWithIdentities<IdentityType> = Auth & {
  identities: IdentityType[]
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
    getFirstProviderUserId: () =>
      data.identities.length > 0 ? data.identities[0].providerUserId : null,
  };
}

// PRIVATE API
export function createAuthUserData(
  user: CompleteUserEntityWithAuth,
  sessionProviderId: string,
): AuthUserData {
  const { auth, ...rest } = user
  if (!auth) {
    throw new Error(`🐝 Error: trying to create a user without auth data.
This should never happen, but it did which means there is a bug in the code.`)
  }
  const identities = auth.identities.map((identity) => ({
    providerName: identity.providerName,
    providerUserId: identity.providerUserId,
    claims: parseProviderData(identity.providerClaims),
    data: parseProviderData(identity.providerData),
  }))
  return {
    ...rest,
    sessionProviderId: sessionProviderId as AuthProviderId,
    identities,
  }
}
