{{={= =}=}}
import { {=# isCustomAuthProviderUsed =}canIssueSessions as canProviderIssueSessions, {=/ isCustomAuthProviderUsed =}type AuthProvider } from './types.js'
{=# isCustomAuthProviderUsed =}
{=# isPackageAuthProvider =}
import { createServerAdapter } from '{= serverPackage =}'
import { config, prisma } from '../../index.js'
{=# externalExtendServerConfig.isDefined =}
{=& externalExtendServerConfig.importStatement =}
{=/ externalExtendServerConfig.isDefined =}
{=/ isPackageAuthProvider =}
{=^ isPackageAuthProvider =}
{=& authProvider.importStatement =}
{=/ isPackageAuthProvider =}
{=/ isCustomAuthProviderUsed =}
{=^ isCustomAuthProviderUsed =}
import { waspAuthProvider } from './wasp.js'
{=/ isCustomAuthProviderUsed =}

// PRIVATE API
export {
  type AuthProvider,
  type SessionIssuingAuthProvider,
  type VerifiedSession,
  canIssueSessions,
} from './types.js'

{=# isPackageAuthProvider =}
/**
 * The adapter package's server factory, called with everything it may know
 * about the app. This runtime object is the adapter's *only* window into the
 * app: adapters never import generated code and never read `process.env`
 * themselves, which is what lets them version independently of any app.
 */
const serverAdapter = await Promise.resolve(
  createServerAdapter(
    {
      db: prisma,
      dbProvider: '{= dbProvider =}',
      env: process.env,
      serverUrl: config.serverUrl,
      clientUrl: config.frontendUrl,
    },
    {=& optionsJson =},
    {
      // The user's escape hatch into the adapter's underlying library
      // configuration; the adapter applies it over its own defaults.
      extendServerConfig: {=# externalExtendServerConfig.isDefined =}{= externalExtendServerConfig.importIdentifier =}{=/ externalExtendServerConfig.isDefined =}{=^ externalExtendServerConfig.isDefined =}undefined{=/ externalExtendServerConfig.isDefined =},
    },
  ),
)

{=/ isPackageAuthProvider =}
// PRIVATE API
/**
 * The auth provider this app runs on.
 *
 * Everything else in Wasp depends on the `AuthProvider` interface rather than on
 * a concrete implementation, so selecting a different one here is the only change
 * needed to authenticate against something other than Wasp's own auth.
 */
export const authProvider: AuthProvider =
  {=# isCustomAuthProviderUsed =}{=# isPackageAuthProvider =}serverAdapter.provider{=/ isPackageAuthProvider =}{=^ isPackageAuthProvider =}{= authProvider.importIdentifier =}{=/ isPackageAuthProvider =}{=/ isCustomAuthProviderUsed =}{=^ isCustomAuthProviderUsed =}waspAuthProvider{=/ isCustomAuthProviderUsed =}
{=# isPackageAuthProvider =}

// PRIVATE API
/**
 * Node handler for the provider's own routes, if it brought any. The server
 * mounts it at the basePath the manifest declared.
 */
export const authProviderRouteHandler = serverAdapter.routeHandler
{=/ isPackageAuthProvider =}
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
