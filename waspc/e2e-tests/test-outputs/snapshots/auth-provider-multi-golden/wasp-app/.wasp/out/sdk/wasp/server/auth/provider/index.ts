import { canManageSessions as canProviderManageSessions, canRevokeSessions as canProviderRevokeSessions, type AuthProvider } from './types.js'
import type { AuthProviderId, ExternalAuthProviderId } from '../../../auth/provider.js'
import type { WaspServerRuntime } from '@wasp.sh/auth-contract'
import { provisionAuthUser } from '../session.js'
import { getIdentityStore } from '../identityStore.js'
import { config, prisma } from '../../index.js'
import { createServerAdapter as createServerAdapter_0 } from '@wasp.sh/auth-clerk/server'
import { waspAuthProvider } from './wasp.js'

// PRIVATE API
export {
  type AuthProvider,
  type SessionManagingAuthProvider,
  type SupportsSessionRevocation,
  type VerifiedSession,
  canManageSessions,
  canRevokeSessions,
} from './types.js'

/**
 * The runtime window an adapter package gets, with the identity store
 * pre-bound to that provider's id. This runtime object is the adapter's *only*
 * window into the app: adapters never import generated code and never read
 * `process.env` themselves, which is what lets them version independently of
 * any app. `provision` routes through that provider's `userSignupFields`,
 * exactly like just-in-time provisioning at the login exchange. The casts are
 * the runtime boundary: the store speaks `unknown`, the contract speaks
 * `JsonValue`, and both sides of every value are plain parsed JSON.
 */
function makeAdapterRuntime(providerId: ExternalAuthProviderId): WaspServerRuntime {
  const identities = getIdentityStore(providerId)
  return {
    db: prisma,
    dbProvider: 'sqlite',
    env: process.env,
    serverUrl: config.serverUrl,
    clientUrl: config.frontendUrl,
    identities: {
      provision: (subjectId, identity) =>
        provisionAuthUser(providerId, subjectId, identity?.claims, {
          data: identity?.data,
          secrets: identity?.secrets,
        }),
      find: (subjectId) => identities.find(subjectId) as any,
      updateData: (subjectId, updates) =>
        identities.updateData(subjectId, updates),
      getSecrets: (subjectId) => identities.getSecrets(subjectId) as any,
      setSecrets: (subjectId, secrets) =>
        identities.setSecrets(subjectId, secrets),
    },
  }
}

/**
 * The adapter package's server factory for 'external:clerk', called with
 * everything it may know about the app.
 */
const serverAdapter_0 = await Promise.resolve(
  createServerAdapter_0(
    makeAdapterRuntime('external:clerk'),
    undefined,
    {
      // The user's setup function for the adapter's underlying library; the
      // adapter calls it with its integration config and uses the result.
      setupFn: undefined,
    },
  ),
)

// PRIVATE API
/**
 * The app's auth providers, keyed by provider id, in `main.wasp.ts`
 * declaration order.
 *
 * Everything else in Wasp depends on the `AuthProvider` interface rather than
 * on concrete implementations. Every provider a session can name is here, so
 * looking up a session's minting provider always succeeds.
 */
export const authProviders: { readonly [Id in AuthProviderId]: AuthProvider } = {
  'wasp': waspAuthProvider,
  'external:clerk': serverAdapter_0.provider,
}

// PRIVATE API
/**
 * The external providers a credential can be exchanged with (`POST
 * /auth/login/:providerId`). Deliberately excludes 'wasp': Wasp's own auth
 * mints sessions through its own routes, and exchanging a Wasp credential for
 * a Wasp session would be a loop.
 */
export const externalAuthProviders: { readonly [Id in ExternalAuthProviderId]: AuthProvider } = {
  'external:clerk': authProviders['external:clerk'],
}

// PRIVATE API
export function getAuthProvider(providerId: string): AuthProvider | undefined {
  return (authProviders as Record<string, AuthProvider>)[providerId]
}

// PRIVATE API
/**
 * Node handlers for the routes external providers brought with them, keyed by
 * provider id. The server mounts each at the basePath its manifest declared.
 */
export const authProviderRouteHandlers: Partial<Record<ExternalAuthProviderId, (req: import('node:http').IncomingMessage, res: import('node:http').ServerResponse) => void | Promise<void>>> = {
  'external:clerk': serverAdapter_0.routeHandler,
}

/**
 * Each manifest in `main.wasp.ts` made compile-time claims about its provider
 * (its id, its capabilities), and code was generated from them. Checking the
 * claims against the adapter objects at boot turns a wrong manifest into a
 * loud startup failure instead of a subtly broken app.
 */
function assertProvidersMatchManifests(): void {
  const manifests: Array<{ providerId: string; capabilities: string[] }> = [
    { providerId: 'external:clerk', capabilities: ['session-revocation'] },
  ]

  const errors: string[] = []

  for (const manifest of manifests) {
    const provider = getAuthProvider(manifest.providerId)
    if (provider === undefined) {
      continue
    }

    if (provider.id !== manifest.providerId) {
      errors.push(
        `the manifest declares id '${manifest.providerId}', but the adapter's id is '${provider.id}' -- ` +
          `identities are recorded under the provider id, so the two must match`,
      )
    }

    if (
      manifest.capabilities.includes('issue-sessions') &&
      !canProviderManageSessions(provider)
    ) {
      errors.push(
        `the manifest for '${manifest.providerId}' declares the 'issue-sessions' capability, but the adapter does not implement the full ` +
          `issueSession/revokeSession/revokeAllSessions set Wasp requires for session management`,
      )
    }

    if (
      manifest.capabilities.includes('session-revocation') &&
      !canProviderRevokeSessions(provider)
    ) {
      errors.push(
        `the manifest for '${manifest.providerId}' declares the 'session-revocation' capability, but the adapter does not implement revokeSession`,
      )
    }
  }

  if (errors.length > 0) {
    throw new Error(
      'Auth provider adapters do not match their manifests:\n' +
        errors.map((error) => `  - ${error}`).join('\n'),
    )
  }
}

assertProvidersMatchManifests()
