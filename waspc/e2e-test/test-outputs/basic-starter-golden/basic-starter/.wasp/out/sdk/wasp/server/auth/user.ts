import {
  type User,
  type Auth,
  type AuthIdentity,
} from '../../entities/index.js'
import {
  type PossibleProviderData,
  getProviderData,
} from '../../auth/utils.js'
import { type ProviderName } from '../_types/index.js'
import { Expand } from '../../universal/types.js'

// PUBLIC API
export type AuthUser = AuthUserData & {
  getFirstProviderUserId: () => string | null,
}

// PRIVATE API
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
  identities: {
    email: Expand<UserFacingProviderData<'email'>> | null
  },
}

type UserFacingProviderData<PN extends ProviderName> = {
  id: string
} & Omit<PossibleProviderData[PN], 'hashedPassword'>

// PRIVATE API
export type CompleteUserEntityWithAuth =
  MakeUserEntityWithAuth<CompleteAuthEntityWithIdentities>

// PRIVATE API
export type CompleteAuthEntityWithIdentities =
  MakeAuthEntityWithIdentities<AuthIdentity>

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

// PRIVATE API
export function createAuthUserData(user: CompleteUserEntityWithAuth): AuthUserData {
  const { auth, ...rest } = user
  if (!auth) {
    throw new Error(`🐝 Error: trying to create a user without auth data.
This should never happen, but it did which means there is a bug in the code.`)
  }
  const identities = {
    email: getProviderInfo<'email'>(auth, 'email'),
  }
  return {
    ...rest,
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
    ...getProviderData<PN>(identity.providerData),
    id: identity.providerUserId,
  }
}

function getIdentity(
  auth: CompleteAuthEntityWithIdentities,
  providerName: ProviderName
): AuthIdentity | null {
  return auth.identities.find((i) => i.providerName === providerName) ?? null
}
