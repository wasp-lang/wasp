export {
  defineUserSignupFields,
} from '../../auth/providers/types.js'

// The identity store: the one channel for reading and writing auth
// identities. Wasp's own auth flows use the exact same facet a user-made
// provider gets -- no privileged access.
export {
  getIdentityStore,
  type Identity,
  type IdentityStore,
  type CreateUserResult,
} from './identityStore.js'

export { createInvalidCredentialsError } from './utils.js'

// The app-level lifecycle hooks (`auth.hooks`) fire for EVERY provider, so
// their types exist for every provider mix.
export type {
  OnBeforeSignupHook,
  OnAfterSignupHook,
  OnBeforeLoginHook,
  OnAfterLoginHook,
  InternalAuthHookParams,
  OAuthData,
} from './hooks.js'



