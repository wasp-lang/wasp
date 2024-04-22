import { type AuthIdentity } from '../entities/index.js'
import { type ProviderName } from '../server/_types/index.js'
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

function findUserIdentity(user: UserEntityWithAuth, providerName: ProviderName): AuthIdentity | null {
  if (!user.auth) {
    return null;
  }
  return user.auth.identities.find(
    (identity) => identity.providerName === providerName
  ) ?? null;
}
