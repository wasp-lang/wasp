import { type AuthProvider } from '@wasp.sh/auth-contract'
import { type FromRegister } from '../../../types/register.js'

// PRIVATE API
/**
 * The contract between Wasp and an authentication provider.
 *
 * The contract itself lives in the `@wasp.sh/auth-contract` package, so that
 * adapter packages can implement it as a normal npm dependency rather than
 * against code Wasp generates into someone's project. This module re-exports it
 * for Wasp's own internals and for adapters written inside the app.
 */
export {
  canIssueSessions,
  type AuthProvider,
  type SessionIssuingAuthProvider,
  type VerifiedSession,
} from '@wasp.sh/auth-contract'

// PRIVATE API
/**
 * The provider the developer registered via `app.auth.provider`, if any.
 *
 * Declared here so that a user-written adapter is type-checked against the
 * contract at build time rather than failing somewhere inside the session layer.
 */
export type RegisteredAuthProvider = FromRegister<'authProvider', AuthProvider>
