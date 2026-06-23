{{={= =}=}}
export * from './config'
export {
  DiscordIcon,
  GitHubIcon,
  GoogleIcon,
  KeycloakIcon,
  MicrosoftIcon,
  SlackIcon,
  getDefaultErrorMessage,
  providerIconById,
  useForgotPasswordForm,
  useLoginForm,
  useOAuthProviderActions,
  useResetPasswordForm,
  useSignupForm,
  useVerifyEmail,
} from '@wasp.sh/lib-auth/browser'
export type {
  AuthFieldErrors,
  AuthFormFieldChangeEvent,
  AuthFormFieldProps,
  AuthFormFields,
  AuthFormStatus,
  AuthFormSubmit,
  AuthFormSubmitEvent,
  AuthFormSubmitOutcome,
  AuthFormSubmitResult,
  AuthFormSuccessMessage,
  AuthFormValidationResult,
  AuthFormValidator,
  AuthIdentityFieldName,
  ErrorMessage,
  ForgotPasswordFormFields,
  ForgotPasswordFormOptions,
  OAuthProvider,
  OAuthProviderAction,
  OAuthProviderId,
  PasswordAuthFormOptions,
  PasswordAuthLabels,
  ProviderIconProps,
  ResetPasswordFormFields,
  ResetPasswordFormOptions,
  ResetPasswordSubmitFields,
  UseOAuthProviderActionsOptions,
  VerifyEmailFormOptions,
} from '@wasp.sh/lib-auth/browser'
{=# isEmailAuthEnabled =}
export * from './email'
{=/ isEmailAuthEnabled =}
{=# isUsernameAndPasswordAuthEnabled =}
export * from './username'
{=/ isUsernameAndPasswordAuthEnabled =}
{=# isSlackAuthEnabled =}
export * from './slack'
{=/ isSlackAuthEnabled =}
{=# isDiscordAuthEnabled =}
export * from './discord'
{=/ isDiscordAuthEnabled =}
{=# isGoogleAuthEnabled  =}
export * from './google'
{=/ isGoogleAuthEnabled =}
{=# isKeycloakAuthEnabled  =}
export * from './keycloak'
{=/ isKeycloakAuthEnabled =}
{=# isGitHubAuthEnabled =}
export * from './github'
{=/ isGitHubAuthEnabled =}
{=# isMicrosoftAuthEnabled =}
export * from './microsoft'
{=/ isMicrosoftAuthEnabled =}
export {
  default as useAuth,
  getMe,
} from '../../auth/useAuth'

export { default as logout } from '../../auth/logout'
