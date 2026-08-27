{{={= =}=}}
import { {=# isCustomAuthProviderUsed =}canManageSessions as canProviderManagesSessions, canRevokeSessions as canProviderRevokeSessions, {=/ isCustomAuthProviderUsed =}type AuthProvider } from './types.js'
{=# isCustomAuthProviderUsed =}
{=# isPackageAuthProvider =}
import { provisionAuthUser } from '../session.js'
import { getIdentityStore } from '../identityStore.js'
import { createServerAdapter } from '{= serverPackage =}'
import { config, prisma } from '../../index.js'
{=# externalSetupFn.isDefined =}
{=& externalSetupFn.importStatement =}
{=/ externalSetupFn.isDefined =}
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
  type SessionManagingAuthProvider,
  type SupportsSessionRevocation,
  type VerifiedSession,
  canManageSessions,
  canRevokeSessions,
} from './types.js'

{=# isPackageAuthProvider =}
const manifestIdentities = getIdentityStore('{= manifestProviderId =}')

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
      // The identity store, pre-bound to this provider's manifest id -- the
      // adapter's sanctioned channel for everything identity-shaped, with the
      // same powers Wasp's own auth flows use. `provision` routes through the
      // app's `userSignupFields`, exactly like just-in-time provisioning at
      // the login exchange. The casts are the runtime boundary: the store
      // speaks `unknown`, the contract speaks `JsonValue`, and both sides of
      // every value are plain parsed JSON.
      identities: {
        provision: (subjectId, identity) =>
          provisionAuthUser(subjectId, identity?.claims, {
            data: identity?.data,
            secrets: identity?.secrets,
          }),
        find: (subjectId) => manifestIdentities.find(subjectId) as any,
        updateData: (subjectId, updates) =>
          manifestIdentities.updateData(subjectId, updates),
        getSecrets: (subjectId) =>
          manifestIdentities.getSecrets(subjectId) as any,
        setSecrets: (subjectId, secrets) =>
          manifestIdentities.setSecrets(subjectId, secrets),
      },
    },
    {=& optionsJson =},
    {
      // The user's setup function for the adapter's underlying library; the
      // adapter calls it with its integration config and uses the result.
      setupFn: {=# externalSetupFn.isDefined =}{= externalSetupFn.importIdentifier =}{=/ externalSetupFn.isDefined =}{=^ externalSetupFn.isDefined =}undefined{=/ externalSetupFn.isDefined =},
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
