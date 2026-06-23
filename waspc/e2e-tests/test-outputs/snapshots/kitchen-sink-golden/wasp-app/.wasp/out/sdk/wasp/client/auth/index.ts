export * from './ui'
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
export * from './email'
export * from './slack'
export * from './discord'
export * from './google'
export * from './github'
export * from './microsoft'
export {
  default as useAuth,
  getMe,
} from '../../auth/useAuth'

export { default as logout } from '../../auth/logout'
