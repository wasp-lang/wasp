{{={= =}=}}
import {
  type {= authIdentityEntityName =},
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
  const { {= authFieldOnUserEntityName =}, ...rest } = user
  if (!{= authFieldOnUserEntityName =}) {
    throw new Error(`🐝 Error: trying to create a user without auth data.
This should never happen, but it did which means there is a bug in the code.`)
  }
  const identities = {
    {=# enabledProviders.isEmailAuthEnabled =}
    email: getProviderInfo<'email'>({= authFieldOnUserEntityName =}, 'email'),
    {=/ enabledProviders.isEmailAuthEnabled =}
    {=# enabledProviders.isUsernameAndPasswordAuthEnabled =}
    username: getProviderInfo<'username'>({= authFieldOnUserEntityName =}, 'username'),
    {=/ enabledProviders.isUsernameAndPasswordAuthEnabled =}
    {=# enabledProviders.isSlackAuthEnabled =}
    slack: getProviderInfo<'slack'>({= authFieldOnUserEntityName =}, 'slack'),
    {=/ enabledProviders.isSlackAuthEnabled =}
    {=# enabledProviders.isDiscordAuthEnabled =}
    discord: getProviderInfo<'discord'>({= authFieldOnUserEntityName =}, 'discord'),
    {=/ enabledProviders.isDiscordAuthEnabled =}
    {=# enabledProviders.isGoogleAuthEnabled =}
    google: getProviderInfo<'google'>({= authFieldOnUserEntityName =}, 'google'),
    {=/ enabledProviders.isGoogleAuthEnabled =}
    {=# enabledProviders.isKeycloakAuthEnabled =}
    keycloak: getProviderInfo<'keycloak'>({= authFieldOnUserEntityName =}, 'keycloak'),
    {=/ enabledProviders.isKeycloakAuthEnabled =}
    {=# enabledProviders.isGitHubAuthEnabled =}
    github: getProviderInfo<'github'>({= authFieldOnUserEntityName =}, 'github'),
    {=/ enabledProviders.isGitHubAuthEnabled =}
    {=# enabledProviders.isMicrosoftAuthEnabled =}
    microsoft: getProviderInfo<'microsoft'>({= authFieldOnUserEntityName =}, 'microsoft'),
    {=/ enabledProviders.isMicrosoftAuthEnabled =}
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
): {= authIdentityEntityName =} | null {
  return auth.{= identitiesFieldOnAuthEntityName =}.find((i) => i.providerName === providerName) ?? null
}
