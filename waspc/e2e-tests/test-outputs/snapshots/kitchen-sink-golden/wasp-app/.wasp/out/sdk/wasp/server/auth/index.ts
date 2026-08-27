export {
  defineUserSignupFields,
} from '../../auth/providers/types.js'

export {
  createProviderId,
  parseProviderData,
  parseProviderSecrets,
  updateAuthIdentityProviderData,
  setAuthIdentitySecrets,
  findAuthIdentity,
  findAuthIdentitySecrets,
  createUser,
  type AuthIdentityWithoutSecrets,
  type CreateUserResult,
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
  OnBeforeSignupHook,
  OnAfterSignupHook,
  OnAfterEmailVerifiedHook,
  OnBeforeOAuthRedirectHook,
  OnBeforeLoginHook,
  OnAfterLoginHook,
  InternalAuthHookParams,
  OAuthData,
} from './hooks.js'

export * from './oauth/index.js'

export * from './email/index.js'

