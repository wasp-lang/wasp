// PRIVATE API
/**
 * The contract between Wasp and an auth handler.
 *
 * The contract itself lives in the `@wasp.sh/auth-contract` package, so that
 * handler packages can implement it as a normal npm dependency rather than
 * against code Wasp generates into someone's project. This module re-exports it
 * for Wasp's own internals and for handlers written inside the app.
 */
export {
  getAuthContractErrorCode,
  type AuthContractErrorCode,
  type AuthenticateResult,
  type AuthHandler,
  type AuthResponse,
  type CredentialRecord,
  type Credentials,
  type CredentialStore,
  type Principal,
  type ProviderIdentities,
  type RuntimeGrantName,
  type SignInContext,
  type SignInProperties,
  type SignInResult,
  type Subject,
  type WaspEmail,
  type WaspServerRuntime,
} from '@wasp.sh/auth-contract'

// PRIVATE API
/**
 * The type the SDK expects of the user's `setupFn` for a handler package's
 * underlying library (the `prismaSetupFn` convention). The handler package
 * types its parameter precisely; the SDK only needs *a* function it can hand
 * to the handler's server factory.
 */
export type AuthProviderSetupFn = NonNullable<
  import('@wasp.sh/auth-contract').ServerAdapterExtensions['setupFn']
>

// PRIVATE API
/**
 * A user function a handler's manifest referenced under `extensions` -- a
 * signup field getter, an OAuth config function, an email content function,
 * a method-specific hook. The handler types each precisely; the SDK only
 * forwards them, so their virtual modules are declared loosely.
 */
export type AuthProviderExtension = unknown
