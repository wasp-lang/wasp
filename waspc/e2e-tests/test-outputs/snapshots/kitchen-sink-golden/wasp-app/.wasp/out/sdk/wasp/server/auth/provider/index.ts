import { type AuthProvider } from './types.js'
import { waspAuthProvider } from './wasp.js'

// PRIVATE API
export {
  type AuthProvider,
  type SessionIssuingAuthProvider,
  type VerifiedSession,
  canIssueSessions,
} from './types.js'

// PRIVATE API
/**
 * The auth provider this app runs on.
 *
 * Everything else in Wasp depends on the `AuthProvider` interface rather than on
 * a concrete implementation, so selecting a different one here is the only change
 * needed to authenticate against something other than Wasp's own auth.
 */
export const authProvider: AuthProvider =
  waspAuthProvider

// PRIVATE API
/**
 * Whether the provider owns Wasp's auth entity.
 *
 * Wasp's own auth writes the `Auth` table itself, so a subject id from it already
 * identifies a local row. An external provider's subject id is foreign, and Wasp
 * has to resolve it to a local user -- provisioning one on first sight.
 */
export const providerOwnsAuthEntity: boolean = true
