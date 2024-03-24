{{={= =}=}}
import {
  type {= userEntityName =},
  type {= authEntityName =},
  type {= authIdentityEntityName =},
} from 'wasp/entities'
import { type ProviderName } from './types'
import { type PossibleProviderData, deserializeAndSanitizeProviderData } from './utils.js'

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
export function findUserIdentity(user: UserEntityWithAuth, providerName: ProviderName): {= authIdentityEntityName =} | undefined {
  return user.auth.identities.find(
    (identity) => identity.providerName === providerName
  );
}

// PUBLIC API
export type AuthUser = ReturnType<typeof createAuthUser>

// PRIVATE API
export function createAuthUser(
  user: UserEntityWithAuth
) {
  const { auth, ...rest } = user
  const identities = {
    {=# enabledProviders.isEmailAuthEnabled =}
    email: getProviderInfo<'email'>(auth, 'email'),
    {=/ enabledProviders.isEmailAuthEnabled =}
    {=# enabledProviders.isUsernameAndPasswordAuthEnabled =}
    username: getProviderInfo<'username'>(auth, 'username'),
    {=/ enabledProviders.isUsernameAndPasswordAuthEnabled =}
    {=# enabledProviders.isGoogleAuthEnabled =}
    google: getProviderInfo<'google'>(auth, 'google'),
    {=/ enabledProviders.isGoogleAuthEnabled =}
    {=# enabledProviders.isKeycloakAuthEnabled =}
    keycloak: getProviderInfo<'keycloak'>(auth, 'keycloak'),
    {=/ enabledProviders.isKeycloakAuthEnabled =}
    {=# enabledProviders.isGitHubAuthEnabled =}
    github: getProviderInfo<'github'>(auth, 'github'),
    {=/ enabledProviders.isGitHubAuthEnabled =}
  }
  return {
    ...rest,
    identities,
    getFirstProviderUserId: () => getFirstProviderUserId(user),
    // Maybe useful for backwards compatibility? Full access?
    _rawUser: user,
  }
}

type UserEntityWithAuth = {= userEntityName =} & {
  {= authFieldOnUserEntityName =}: AuthEntityWithIdentities
}

type AuthEntityWithIdentities = {= authEntityName =} & {
  {= identitiesFieldOnAuthEntityName =}: {= authIdentityEntityName =}[]
}

function getProviderInfo<PN extends ProviderName>(
  auth: AuthEntityWithIdentities,
  providerName: PN
):
  | {
      id: string
      data: PossibleProviderData[PN]
    }
  | null {
  const identity = getIdentity(auth, providerName)
  if (!identity) {
    return null
  }
  return {
    id: identity.providerUserId,
    data: deserializeAndSanitizeProviderData<PN>(identity.providerData, {
      shouldRemovePasswordField: true,
    }),
  }
}

function getIdentity(
  auth: AuthEntityWithIdentities,
  providerName: ProviderName
): {= authIdentityEntityName =} | null {
  return auth.{= identitiesFieldOnAuthEntityName =}.find((i) => i.providerName === providerName) ?? null
}
