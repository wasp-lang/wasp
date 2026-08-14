import { waspAuthProvider } from './wasp.js'
import { type AuthProvider } from './types.js'

// PRIVATE API
export {
  type AuthProvider,
  type SessionManagingAuthProvider,
  type VerifiedSession,
  canManageSessions,
} from './types.js'

// PRIVATE API
/**
 * The auth provider this app runs on.
 *
 * There is exactly one today and it is not configurable. This module exists so that
 * everything else in Wasp depends on the `AuthProvider` interface rather than on a
 * concrete implementation -- making the provider selectable is a later, additive
 * change that will not touch any of its consumers.
 */
export const authProvider: AuthProvider = waspAuthProvider
