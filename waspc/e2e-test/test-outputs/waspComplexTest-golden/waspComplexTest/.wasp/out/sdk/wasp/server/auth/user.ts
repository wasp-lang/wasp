import {
  type User,
  type Auth,
  type AuthIdentity,
} from '../../entities/index.js'
import {
  type PossibleProviderData,
  deserializeAndSanitizeProviderData
} from '../../auth/utils.js'
import { type ProviderName } from '../_types/index.js'
import { getFirstProviderUserId } from '../../auth/user.js'
import { Expand } from '../../universal/types.js'

// PUBLIC API
/**
 * Ideally, we'd do something like this:
 * ```
 * export type AuthUser = ReturnType<typeof createAuthUser>
 * ```
 * to get the benefits of the createAuthUser and the AuthUser type being in sync.
 * 
 * But since we are not using strict mode, the inferred return type of createAuthUser
 * is not correct. So we have to define the AuthUser type manually.
 * 
 * TODO: Change this once/if we switch to strict mode. https://github.com/wasp-lang/wasp/issues/1938
 */
export type AuthUser = Omit<UserEntityWithAuth, 'auth'> & {
  identities: {
    google: Expand<UserFacingProviderData<'google'>> | null
  },
  getFirstProviderUserId: () => string | null,
}

type UserFacingProviderData<PN extends ProviderName> = {
  id: string
} & Omit<PossibleProviderData[PN], 'hashedPassword'>

// PRIVATE API
export type UserEntityWithAuth = User & {
  auth: AuthEntityWithIdentities | null
}

// PRIVATE API
export type AuthEntityWithIdentities = Auth & {
  identities: AuthIdentity[]
}

// PRIVATE API
export function createAuthUser(user: UserEntityWithAuth): AuthUser {
  const { auth, ...rest } = user
  if (!auth) {
    throw new Error(`🐝 Error: trying to create a user without auth data.
This should never happen, but it did which means there is a bug in the code.`)
  }
  const identities = {
    google: getProviderInfo<'google'>(auth, 'google'),
  }
  return {
    ...rest,
    identities,
    getFirstProviderUserId: () => getFirstProviderUserId(user),
  }
}

function getProviderInfo<PN extends ProviderName>(
  auth: AuthEntityWithIdentities,
  providerName: PN
):
  | UserFacingProviderData<PN>
  | null {
  const identity = getIdentity(auth, providerName)
  if (!identity) {
    return null
  }
  return {
    ...deserializeAndSanitizeProviderData<PN>(identity.providerData, {
      shouldRemovePasswordField: true,
    }),
    id: identity.providerUserId,
  }
}

function getIdentity(
  auth: AuthEntityWithIdentities,
  providerName: ProviderName
): AuthIdentity | null {
  return auth.identities.find((i) => i.providerName === providerName) ?? null
}
