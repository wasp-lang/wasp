{{={= =}=}}
export {
  defineUserSignupFields,
} from '../../auth/providers/types.js'

{=# isCustomAuthProviderUsed =}
{=! Under an external provider, `wasp/server/auth` keeps only what is
    provider-independent: typing `userSignupFields` and the error helper the
    generated code itself uses. Everything password- and hook-shaped belongs
    to Wasp's own auth and is not generated at all. =}
export { createInvalidCredentialsError } from './utils.js'
{=/ isCustomAuthProviderUsed =}
{=^ isCustomAuthProviderUsed =}
export {
  createProviderId,
  sanitizeAndSerializeProviderData,
  updateAuthIdentityProviderData,
  getProviderData,
  getProviderDataWithPassword,
  findAuthIdentity,
  createUser,
  type CreateUserResult,
  type ProviderId,
  type ProviderName,
  type EmailProviderData,
  type UsernameProviderData,
  type OAuthProviderData,
} from './utils.js'

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
{=/ isCustomAuthProviderUsed =}

{=# isExternalAuthEnabled =}
export * from './oauth/index.js'
{=/ isExternalAuthEnabled =}

{=# enabledProviders.isEmailAuthEnabled =}
export * from './email/index.js'
{=/ enabledProviders.isEmailAuthEnabled =}

{=# enabledProviders.isUsernameAndPasswordAuthEnabled =}
export * from './username.js'
{=/ enabledProviders.isUsernameAndPasswordAuthEnabled =}
