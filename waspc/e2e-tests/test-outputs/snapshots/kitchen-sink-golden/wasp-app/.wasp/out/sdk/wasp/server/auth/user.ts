import {
  type AuthIdentity,
} from '../../entities/index.js'
import {
  getProviderData,
  type ProviderName,
} from '../../auth/utils.js'
import type {
  AuthUserData,
  CompleteUserEntityWithAuth,
  CompleteAuthEntityWithIdentities,
  UserFacingProviderData,
} from '../../auth/user.js'

/**
 * FIXME: https://github.com/wasp-lang/wasp/issues/4527 - bad code split.
 * This module contains the server runtime part.
 * The runtime agnostic part lives in `auth/` dir.
 */

// PRIVATE API
export function createAuthUserData(user: CompleteUserEntityWithAuth): AuthUserData {
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
