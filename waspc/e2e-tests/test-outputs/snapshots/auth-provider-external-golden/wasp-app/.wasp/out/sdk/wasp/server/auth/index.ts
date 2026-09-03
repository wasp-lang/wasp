export {
  defineUserSignupFields,
} from '../../auth/providers/types.js'

// The identity store: the one channel for reading and writing auth
// identities. Every auth provider package uses the exact same facet a
// hand-written provider gets -- no privileged access.
export {
  getIdentityStore,
  type Identity,
  type IdentityStore,
  type CreateUserResult,
} from './identityStore.js'

export { createInvalidCredentialsError } from './utils.js'

export {
  createProviderId,
  parseProviderData,
  parseProviderSecrets,
  type ProviderId,
  type ProviderName,
} from './utils.js'

// The app-level lifecycle hooks (`auth.hooks`) fire for EVERY provider, so
// their types exist for every provider mix.
export type {
  OnBeforeSignupHook,
  OnAfterSignupHook,
  OnBeforeLoginHook,
  OnAfterLoginHook,
  InternalAuthHookParams,
} from './hooks.js'
