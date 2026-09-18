// PUBLIC API
/**
 * The client half of the contract between Wasp and an auth handler, for
 * handlers written inside the app: type a hand-written client factory as
 * `ClientAuthHandlerFactory`, exactly like a handler package's
 * `createClientAuthHandler`.
 */
export type {
  ClientAuthHandlerFactory,
  ClientAuthHandler,
  WaspClientRuntime,
} from '@wasp.sh/auth-contract/client'

// PRIVATE API
/**
 * App code a handler's `client.config` references (a component, a callback).
 * The handler types each precisely; the SDK only sets them back into the
 * config it hands the factory, so their virtual modules are declared loosely.
 */
export type AuthHandlerConfigReference = unknown
