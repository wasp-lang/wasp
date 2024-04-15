{{={= =}=}}
import {
  type {= userEntityName =},
  type {= authEntityName =},
  type {= authIdentityEntityName =},
} from 'wasp/entities'
import type { ProviderName, DeserializedAuthIdentity } from './types'
import { type PossibleProviderData, deserializeAndSanitizeProviderData } from './utils.js'

// PUBLIC API
export function getEmail(user: FullUser): string | null {
  return findUserIdentity(user, "email")?.providerUserId ?? null;
}

// PUBLIC API
export function getUsername(user: FullUser): string | null {
  return findUserIdentity(user, "username")?.providerUserId ?? null;
}

// PUBLIC API
export function getFirstProviderUserId(user?: FullUser): string | null {
  if (!user || !user.auth || !user.auth.identities || user.auth.identities.length === 0) {
    return null;
  }

  return user.auth.identities[0].providerUserId ?? null;
}

// PUBLIC API
export function findUserIdentity(user: FullUser, providerName: ProviderName): DeserializedAuthIdentity | undefined {
  return user.auth.identities.find(
    (identity) => identity.providerName === providerName
  );
}

export type AuthUser = ReturnType<typeof createAuthUser>

type FullUser = {= userEntityName =} & {
  {= authFieldOnUserEntityName =}: FullAuth
}

type FullAuth = {= authEntityName =} & {
  {= identitiesFieldOnAuthEntityName =}: {= authIdentityEntityName =}[]
}

// PRIVATE API
export function createAuthUser(
  user: FullUser
) {
  const { auth, ...rest } = user
  const identities = {
    email: getProviderInfo<'email'>(auth, 'email'),
    username: getProviderInfo<'username'>(auth, 'username'),
    google: getProviderInfo<'google'>(auth, 'google'),
    keycloak: getProviderInfo<'keycloak'>(auth, 'keycloak'),
    github: getProviderInfo<'github'>(auth, 'github'),
  }
  return {
    ...rest,
    identities,
    getFirstProviderUserId: () => getFirstProviderUserId(user),
    // Maybe useful for backwards compatibility? Full access?
    _rawUser: user,
  }
}

function getProviderInfo<PN extends ProviderName>(
  auth: FullAuth,
  providerName: PN
):
  | {
      id: string
      data: PossibleProviderData[PN]
    }
  | undefined {
  const identity = getIdentity(auth, providerName)
  if (!identity) {
    return undefined
  }
  return {
    id: identity.providerUserId,
    data: deserializeAndSanitizeProviderData<PN>(identity.providerData, {
      shouldRemovePasswordField: true,
    }),
  }
}

function getIdentity(
  auth: FullAuth,
  providerName: ProviderName
): {= authIdentityEntityName =} | undefined {
  return auth.{= identitiesFieldOnAuthEntityName =}.find((i) => i.providerName === providerName)
}

