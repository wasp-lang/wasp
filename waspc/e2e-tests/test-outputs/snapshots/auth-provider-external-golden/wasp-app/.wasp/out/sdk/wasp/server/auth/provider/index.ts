import { canIssueSessions as canProviderIssueSessions, type AuthProvider } from './types.js'
import { clerkAuthProvider as clerkAuthProvider_ext } from 'virtual:wasp/user/auth/provider'

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
  clerkAuthProvider_ext

/**
 * The manifest in `main.wasp.ts` made compile-time claims about this provider
 * (its id, its capabilities), and code was generated from them. Checking the
 * claims against the adapter object at boot turns a wrong manifest into a
 * loud startup failure instead of a subtly broken app.
 */
function assertProviderMatchesManifest(): void {
  const manifestProviderId = "clerk";
  const manifestCapabilities: string[] = ['session-revocation'];

  if (authProvider.id !== manifestProviderId) {
    throw new Error(
      `The auth provider manifest declares id '${manifestProviderId}', but the adapter's id is '${authProvider.id}'. ` +
        `Identities are recorded under the provider id, so the two must match.`,
    );
  }

  if (
    manifestCapabilities.includes('issue-sessions') &&
    !canProviderIssueSessions(authProvider)
  ) {
    throw new Error(
      `The auth provider manifest for '${manifestProviderId}' declares the 'issue-sessions' capability, but the adapter does not implement issueSession/revokeAllSessions.`,
    );
  }

  if (
    manifestCapabilities.includes('session-revocation') &&
    typeof authProvider.revokeSession !== 'function'
  ) {
    throw new Error(
      `The auth provider manifest for '${manifestProviderId}' declares the 'session-revocation' capability, but the adapter does not implement revokeSession.`,
    );
  }
}

assertProviderMatchesManifest()

// PRIVATE API
/**
 * Whether the provider owns Wasp's auth entity.
 *
 * Wasp's own auth writes the `Auth` table itself, so a subject id from it already
 * identifies a local row. An external provider's subject id is foreign, and Wasp
 * has to resolve it to a local user -- provisioning one on first sight.
 */
export const providerOwnsAuthEntity: boolean = false
