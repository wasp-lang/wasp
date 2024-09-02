export {
  defineUserSignupFields,
} from '../../auth/providers/types.js'

export {
  createProviderId,
  sanitizeAndSerializeProviderData,
  updateAuthIdentityProviderData,
  deserializeAndSanitizeProviderData,
  findAuthIdentity,
  createUser,
  type ProviderId,
  type ProviderName,
  type EmailProviderData,
  type UsernameProviderData,
  type OAuthProviderData,
} from '../../auth/utils.js'

export {
  ensurePasswordIsPresent,
  ensureValidPassword,
  ensureTokenIsPresent,
} from '../../auth/validation.js'

export type {
  OnBeforeSignupHook,
  OnAfterSignupHook,
  OnBeforeOAuthRedirectHook,
  OnBeforeLoginHook,
  OnAfterLoginHook,
  InternalAuthHookParams,
  OAuthData,
} from './hooks.js'

export * from './oauth/index.js'


