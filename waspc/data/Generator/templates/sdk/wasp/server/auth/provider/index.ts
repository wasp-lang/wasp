{{={= =}=}}
import { {=# isCustomAuthProviderUsed =}canManageSessions as canProviderManagesSessions, canRevokeSessions as canProviderRevokeSessions, {=/ isCustomAuthProviderUsed =}type AuthProvider } from './types.js'
{=# isCustomAuthProviderUsed =}
{=& authProvider.importStatement =}
{=/ isCustomAuthProviderUsed =}
{=^ isCustomAuthProviderUsed =}
import { waspAuthProvider } from './wasp.js'
{=/ isCustomAuthProviderUsed =}

// PRIVATE API
export {
  type AuthProvider,
  type SessionManagingAuthProvider,
  type SupportsSessionRevocation,
  type VerifiedSession,
  canManageSessions,
  canRevokeSessions,
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
  {=# isCustomAuthProviderUsed =}{= authProvider.importIdentifier =}{=/ isCustomAuthProviderUsed =}{=^ isCustomAuthProviderUsed =}waspAuthProvider{=/ isCustomAuthProviderUsed =}
{=# isCustomAuthProviderUsed =}

/**
 * The manifest in `main.wasp.ts` made compile-time claims about this provider
 * (its id, its capabilities), and code was generated from them. Checking the
 * claims against the adapter object at boot turns a wrong manifest into a
 * loud startup failure instead of a subtly broken app.
 */
function assertProviderMatchesManifest(): void {
  const manifestProviderId = "{= manifestProviderId =}";
  const manifestCapabilities: string[] = {=& manifestCapabilities =};

  const errors: string[] = [];

  if (authProvider.id !== manifestProviderId) {
    errors.push(
      `the manifest declares id '${manifestProviderId}', but the adapter's id is '${authProvider.id}' -- ` +
        `identities are recorded under the provider id, so the two must match`,
    );
  }

  if (
    manifestCapabilities.includes('issue-sessions') &&
    !canProviderManagesSessions(authProvider)
  ) {
    errors.push(
      `the manifest declares the 'issue-sessions' capability, but the adapter does not implement the full ` +
        `issueSession/revokeSession/revokeAllSessions set Wasp requires for session management`,
    );
  }

  if (
    manifestCapabilities.includes('session-revocation') &&
    !canProviderRevokeSessions(authProvider)
  ) {
    errors.push(
      `the manifest declares the 'session-revocation' capability, but the adapter does not implement revokeSession`,
    );
  }

  if (errors.length > 0) {
    throw new Error(
      `The auth provider adapter does not match its manifest ('${manifestProviderId}'):\n` +
        errors.map((error) => `  - ${error}`).join('\n'),
    );
  }
}

assertProviderMatchesManifest()
{=/ isCustomAuthProviderUsed =}

// PRIVATE API
/**
 * Whether the provider owns Wasp's auth entity.
 *
 * Wasp's own auth writes the `Auth` table itself, so a subject id from it already
 * identifies a local row. An external provider's subject id is foreign, and Wasp
 * has to resolve it to a local user -- provisioning one on first sight.
 */
export const providerOwnsAuthEntity: boolean = {=# isCustomAuthProviderUsed =}false{=/ isCustomAuthProviderUsed =}{=^ isCustomAuthProviderUsed =}true{=/ isCustomAuthProviderUsed =}
