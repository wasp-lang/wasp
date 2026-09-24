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
  type CredentialHandler,
  type CredentialRecord,
  type CredentialsIssuer,
  type CredentialStore,
  type IdentityPrincipal,
  type AccountPrincipal,
  type IdentityStore,
  type OAuthLoginData,
  type OAuthTokens,
  type RuntimeGrantName,
  type ServerAuthHandlerParts,
  type ServerAuthAdapter,
  type SignInOpts,
  type SignInProperties,
  type SignInResult,
  type ResolvedIdentity,
  type AuthIdentityRef,
  type WaspEmail,
  type WaspServerRuntime,
} from '@wasp.sh/auth-contract'

// PRIVATE API
/**
 * App code a handler's `server.spec` references -- a signup field getter,
 * an OAuth config function, an email content function, a setup function for
 * the handler's underlying library. The handler types each precisely; the SDK
 * only sets them back into the spec it hands the adapter, so their virtual
 * modules are declared loosely.
 */
export type AuthHandlerSpecReference = unknown
