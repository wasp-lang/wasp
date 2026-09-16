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
  canManageSessions,
  canRevokeSessions,
  getAuthContractErrorCode,
  type AuthContractErrorCode,
  type AuthenticateResult,
  type AuthProvider,
  type ProviderIdentities,
  type RuntimeGrantName,
  type SessionManagingAuthProvider,
  type SubjectRef,
  type SupportsAllSessionsRevocation,
  type SupportsSessionIssuance,
  type SupportsSessionRevocation,
  type VerifiedSession,
  type WaspEmail,
  type WaspServerRuntime,
  type WaspSessions,
} from '@wasp.sh/auth-contract'

// PRIVATE API
/**
 * The type the SDK expects of the user's `setupFn` for a packaged adapter's
 * underlying library (the `prismaSetupFn` convention). The adapter package
 * types its parameter precisely; the SDK only needs *a* function it can hand
 * to the adapter's server factory.
 */
export type AuthProviderSetupFn = NonNullable<
  import('@wasp.sh/auth-contract').ServerAdapterExtensions['setupFn']
>
