{{={= =}=}}
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
  OnBeforeSignupHookFn,
  OnAfterSignupHookFn,
  OnBeforeOAuthRedirectHookFn,
  OnAfterOAuthTokenReceivedHookFn,
} from './hooks.js'

{=# isEmailAuthEnabled =}
export * from './email/index.js'
{=/ isEmailAuthEnabled =}

{=# isUsernameAndPasswordAuthEnabled =}
export * from './username.js'
{=/ isUsernameAndPasswordAuthEnabled =}
