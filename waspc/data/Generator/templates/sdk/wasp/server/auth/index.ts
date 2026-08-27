{{={= =}=}}
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

{=# isCustomAuthProviderUsed =}
{=! Under an external provider, `wasp/server/auth` keeps what is
    provider-independent: the identity store, typing `userSignupFields`, and
    the error helper the generated code itself uses. Everything password- and
    hook-shaped belongs to Wasp's own auth and is not generated at all. =}
export { createInvalidCredentialsError } from './utils.js'
{=/ isCustomAuthProviderUsed =}
{=^ isCustomAuthProviderUsed =}
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
