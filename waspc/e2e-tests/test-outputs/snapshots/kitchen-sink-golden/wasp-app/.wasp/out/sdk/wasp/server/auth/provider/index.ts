import { type AuthProvider } from './types.js'
import type { AuthProviderId, ExternalAuthProviderId } from '../../../auth/provider.js'
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
}

// PRIVATE API
/**
 * The external providers a credential can be exchanged with (`POST
 * /auth/login/:providerId`). Deliberately excludes 'wasp': Wasp's own auth
 * mints sessions through its own routes, and exchanging a Wasp credential for
 * a Wasp session would be a loop.
 */
export const externalAuthProviders: { readonly [Id in ExternalAuthProviderId]: AuthProvider } = {
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
}
