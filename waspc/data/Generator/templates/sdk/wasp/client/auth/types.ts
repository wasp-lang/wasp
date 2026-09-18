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
