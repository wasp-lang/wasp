import {
  getEmail as getLibEmail,
  getFirstProviderUserId as getLibFirstProviderUserId,
  getUsername as getLibUsername,
  makeAuthUserIfPossible as makeLibAuthUserIfPossible,
} from '@wasp.sh/lib-auth'
import type {
  AuthUserData,
  AuthUser,
  UserEntityWithAuth,
} from '../server/auth/user.js'

/**
 * We split the user.ts code into two files to avoid some server-only
 * code (Oslo's hashing functions) being imported on the client.
 */

// PUBLIC API
export function getEmail(user: UserEntityWithAuth): string | null {
  return getLibEmail(user);
}

// PUBLIC API
export function getUsername(user: UserEntityWithAuth): string | null {
  return getLibUsername(user);
}

// PUBLIC API
export function getFirstProviderUserId(user?: UserEntityWithAuth): string | null {
  return getLibFirstProviderUserId(user);
}

// PRIVATE API (used in SDK and server)
export type { AuthUserData, AuthUser } from '../server/auth/user.js'

// PRIVATE API (used in SDK and server)
export function makeAuthUserIfPossible(user: null): null
export function makeAuthUserIfPossible(user: AuthUserData): AuthUser
export function makeAuthUserIfPossible(user: AuthUserData | null): AuthUser | null
export function makeAuthUserIfPossible(
  user: AuthUserData | null,
): AuthUser | null {
  return makeLibAuthUserIfPossible(user);
}
