import {
  type User,
  type Auth,
  type AuthIdentity,
} from '../entities/index.js'
import {
  type PossibleProviderData,
  type ProviderName,
  parseProviderData,
} from './providerData.js'
import { Expand } from '../universal/types.js'
import { type AuthProviderId } from './provider.js'
import { isNotNull } from '../universal/predicates.js'

// PUBLIC API
export function getEmail(user: UserEntityWithAuth): string | null {
  return findUserIdentity(user, "email")?.providerUserId ?? null;
}

// PUBLIC API
export function getUsername(user: UserEntityWithAuth): string | null {
  return findUserIdentity(user, "username")?.providerUserId ?? null;
}

// PUBLIC API
export function getFirstProviderUserId(user?: UserEntityWithAuth): string | null {
  if (!user || !user.auth || !user.auth.identities || user.auth.identities.length === 0) {
    return null;
  }

  return user.auth.identities[0].providerUserId ?? null;
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
   * user logged in this time ('wasp', 'external:clerk', ...). A session is
   * always minted by exactly one provider, so this is a single compile-checked
   * literal, pinned when the session was created and never re-derived.
   */
  sessionProviderId: AuthProviderId,
  identities: {
    email: Expand<UserFacingProviderData<'email'>> | null
    slack: Expand<UserFacingProviderData<'slack'>> | null
    discord: Expand<UserFacingProviderData<'discord'>> | null
    google: Expand<UserFacingProviderData<'google'>> | null
    github: Expand<UserFacingProviderData<'github'>> | null
    microsoft: Expand<UserFacingProviderData<'microsoft'>> | null
  },
}

// With secrets in their own (client-omitted) column, the user-facing view is
// the provider's data verbatim -- no field stripping needed or possible.
type UserFacingProviderData<PN extends ProviderName> = {
  id: string
} & PossibleProviderData[PN]

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
 * helper functions like `getUsername` and `getEmail`.
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
    getFirstProviderUserId: () => {
      const identities = Object.values(data.identities).filter(isNotNull);
      return identities.length > 0 ? identities[0].id : null;
    },
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
  const identities = {
    email: getProviderInfo<'email'>(auth, 'email'),
    slack: getProviderInfo<'slack'>(auth, 'slack'),
    discord: getProviderInfo<'discord'>(auth, 'discord'),
    google: getProviderInfo<'google'>(auth, 'google'),
    github: getProviderInfo<'github'>(auth, 'github'),
    microsoft: getProviderInfo<'microsoft'>(auth, 'microsoft'),
  }
  return {
    ...rest,
    sessionProviderId: sessionProviderId as AuthProviderId,
    identities,
  }
}

function getProviderInfo<PN extends ProviderName>(
  auth: CompleteAuthEntityWithIdentities,
  providerName: PN
):
  | UserFacingProviderData<PN>
  | null {
  const identity = getIdentity(auth, providerName)
  if (!identity) {
    return null
  }
  return {
    ...parseProviderData<PN>(identity.providerData),
    id: identity.providerUserId,
  }
}

function getIdentity(
  auth: CompleteAuthEntityWithIdentities,
  providerName: ProviderName
): Omit<AuthIdentity, 'providerSecrets'> | null {
  return auth.identities.find((i) => i.providerName === providerName) ?? null
}

function findUserIdentity(user: UserEntityWithAuth, providerName: ProviderName): NonNullable<UserEntityWithAuth['auth']>['identities'][number] | null {
  if (!user.auth) {
    return null;
  }
  return user.auth.identities.find(
    (identity) => identity.providerName === providerName
  ) ?? null;
}
