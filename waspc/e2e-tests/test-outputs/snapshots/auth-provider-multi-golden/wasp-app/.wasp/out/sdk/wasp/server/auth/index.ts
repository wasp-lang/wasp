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
export {
  createProviderId,
  parseProviderData,
  parseProviderSecrets,
  type ProviderId,
  type ProviderName,
  type EmailProviderData,
  type EmailProviderSecrets,
  type UsernameProviderData,
  type UsernameProviderSecrets,
  type OAuthProviderData,
  type OAuthProviderSecrets,
} from './utils.js'

// Hashing is the caller's explicit job when writing secrets -- exported so
// user-land code (seeds, custom signup actions) can do it the same way the
// generated flows do.
export { hashPassword, verifyPassword } from './password.js'

export {
  ensurePasswordIsPresent,
  ensureValidPassword,
  ensureTokenIsPresent,
} from '../../auth/validation.js'

export type {
  OnAfterEmailVerifiedHook,
  OnBeforeOAuthRedirectHook,
} from './hooks.js'

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


export { ensureValidUsername } from '../../auth/validation.js'
