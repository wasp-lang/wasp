import {
  type User,
  type Auth,
  type AuthIdentity,
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
export function findUserIdentity(user: UserEntityWithAuth, providerName: ProviderName): AuthIdentity | undefined {
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
    google: getProviderInfo<'google'>(auth, 'google'),
  }
  return {
    ...rest,
    identities,
    getFirstProviderUserId: () => getFirstProviderUserId(user),
    // Maybe useful for backwards compatibility? Full access?
    _rawUser: user,
  }
}

type UserEntityWithAuth = User & {
  auth: AuthEntityWithIdentities
}

type AuthEntityWithIdentities = Auth & {
  identities: AuthIdentity[]
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
): AuthIdentity | null {
  return auth.identities.find((i) => i.providerName === providerName) ?? null
}
