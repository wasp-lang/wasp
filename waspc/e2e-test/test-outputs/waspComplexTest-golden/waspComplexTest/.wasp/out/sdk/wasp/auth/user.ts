import { type AuthIdentity } from '../entities/index.js'
import { type ProviderName } from '../server/_types/index.js'
import type { AuthUserData, AuthUser } from '../server/auth/user.js'
/**
 * We split the user.ts code into two files to avoid some server-only
 * code (Oslo's hashing functions) being imported on the client.
 */
import { type UserEntityWithAuth } from '../server/auth/user.js'

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

// PRIVATE API (used in SDK and server)
export type { AuthUserData, AuthUser } from '../server/auth/user.js'

// PRIVATE API (used in SDK and server)
export function makeAuthUserIfPossible(user: null): null
export function makeAuthUserIfPossible(user: AuthUserData): AuthUser
export function makeAuthUserIfPossible(
  user: AuthUserData | null,
): AuthUser | null {
  return user ? makeAuthUser(user) : null
}

function makeAuthUser(data: AuthUserData): AuthUser {
  return {
    ...data,
    getFirstProviderUserId: () => {
      const identities = Object.values(data.identities).filter(Boolean);
      return identities.length > 0 ? identities[0].id : null;
    },
  };
}

function findUserIdentity(user: UserEntityWithAuth, providerName: ProviderName): AuthIdentity | null {
  if (!user.auth) {
    return null;
  }
  return user.auth.identities.find(
    (identity) => identity.providerName === providerName
  ) ?? null;
}
