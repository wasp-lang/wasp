{{={= =}=}}
import {
  type {= userEntityName =},
  type {= authEntityName =},
  type {= authIdentityEntityName =},
} from '../../entities/index.js'
import {
  type PossibleProviderData,
  deserializeAndSanitizeProviderData
} from '../../auth/utils.js'
import { type ProviderName } from '../_types/index.js'
import { Expand } from '../../universal/types.js'

// PUBLIC API
export type AuthUser = AuthUserData & {
  getFirstProviderUserId: () => string | null,
}

// PRIVATE API
/**
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
export type AuthUserData = Omit<UserEntityWithAuth, '{= authFieldOnUserEntityName =}'> & {
  identities: {
    {=# enabledProviders.isEmailAuthEnabled =}
    email: Expand<UserFacingProviderData<'email'>> | null
    {=/ enabledProviders.isEmailAuthEnabled =}
    {=# enabledProviders.isUsernameAndPasswordAuthEnabled =}
    username: Expand<UserFacingProviderData<'username'>> | null
    {=/ enabledProviders.isUsernameAndPasswordAuthEnabled =}
    {=# enabledProviders.isGoogleAuthEnabled =}
    google: Expand<UserFacingProviderData<'google'>> | null
    {=/ enabledProviders.isGoogleAuthEnabled =}
    {=# enabledProviders.isKeycloakAuthEnabled =}
    keycloak: Expand<UserFacingProviderData<'keycloak'>> | null
    {=/ enabledProviders.isKeycloakAuthEnabled =}
    {=# enabledProviders.isGitHubAuthEnabled =}
    github: Expand<UserFacingProviderData<'github'>> | null
    {=/ enabledProviders.isGitHubAuthEnabled =}
  },
}

type UserFacingProviderData<PN extends ProviderName> = {
  id: string
} & Omit<PossibleProviderData[PN], 'hashedPassword'>

// PRIVATE API
export type UserEntityWithAuth = {= userEntityName =} & {
  {= authFieldOnUserEntityName =}: AuthEntityWithIdentities | null
}

// PRIVATE API
export type AuthEntityWithIdentities = {= authEntityName =} & {
  {= identitiesFieldOnAuthEntityName =}: {= authIdentityEntityName =}[]
}

// PRIVATE API
export function createAuthUserData(user: UserEntityWithAuth): AuthUserData {
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
    {=# enabledProviders.isGoogleAuthEnabled =}
    google: getProviderInfo<'google'>({= authFieldOnUserEntityName =}, 'google'),
    {=/ enabledProviders.isGoogleAuthEnabled =}
    {=# enabledProviders.isKeycloakAuthEnabled =}
    keycloak: getProviderInfo<'keycloak'>({= authFieldOnUserEntityName =}, 'keycloak'),
    {=/ enabledProviders.isKeycloakAuthEnabled =}
    {=# enabledProviders.isGitHubAuthEnabled =}
    github: getProviderInfo<'github'>({= authFieldOnUserEntityName =}, 'github'),
    {=/ enabledProviders.isGitHubAuthEnabled =}
  }
  return {
    ...rest,
    identities,
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
): {= authIdentityEntityName =} | null {
  return auth.{= identitiesFieldOnAuthEntityName =}.find((i) => i.providerName === providerName) ?? null
}
