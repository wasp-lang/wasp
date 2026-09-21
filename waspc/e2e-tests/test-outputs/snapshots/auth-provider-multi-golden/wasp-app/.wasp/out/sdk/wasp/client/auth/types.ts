// PUBLIC API
/**
 * The client half of the contract between Wasp and an auth handler, for
 * handlers written inside the app: type a hand-written client adapter as
 * `ClientAuthAdapter`, exactly like a handler package's
 * `createClientAuthHandler`.
 */
export type {
  ClientAuthAdapter,
  ClientAuthHandler,
  WaspClientRuntime,
} from '@wasp.sh/auth-contract/client'

// PRIVATE API
/**
 * App code a handler's `client.spec` references (a component, a callback).
 * The handler types each precisely; the SDK only sets them back into the
 * spec it hands the adapter, so their virtual modules are declared loosely.
 */
export type AuthHandlerSpecReference = unknown
