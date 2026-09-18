// PUBLIC API
/**
 * The client half of the contract between Wasp and an auth handler, for
 * handlers written inside the app: type a hand-written client factory as
 * `ClientAdapterFactory`, exactly like a handler package's
 * `createClientAdapter`.
 */
export type {
  ClientAdapterFactory,
  ClientAuthAdapter,
  WaspClientRuntime,
} from '@wasp.sh/auth-contract/client'
