{{={= =}=}}
{=# isWaspAuthProviderUsed =}
// Wasp's own auth UI and actions come from the @wasp.sh/auth lib, instantiated
// for this app in './providers'.
import './providers'
export {
  LoginForm,
  SignupForm,
  ForgotPasswordForm,
  ResetPasswordForm,
  VerifyEmailForm,
  FormError,
  FormInput,
  FormTextarea,
  FormItemGroup,
  FormLabel,
  SubmitButton,
  type CustomizationOptions,
  login,
  signup,
  requestPasswordReset,
  resetPassword,
  verifyEmail,
} from '@wasp.sh/auth/client'
{=/ isWaspAuthProviderUsed =}
{=# isSlackAuthEnabled =}
export { slackSignInUrl, SlackSignInButton } from '@wasp.sh/auth/client'
{=/ isSlackAuthEnabled =}
{=# isDiscordAuthEnabled =}
export { discordSignInUrl, DiscordSignInButton } from '@wasp.sh/auth/client'
{=/ isDiscordAuthEnabled =}
{=# isGoogleAuthEnabled  =}
export { googleSignInUrl, GoogleSignInButton } from '@wasp.sh/auth/client'
{=/ isGoogleAuthEnabled =}
{=# isKeycloakAuthEnabled  =}
export { keycloakSignInUrl, KeycloakSignInButton } from '@wasp.sh/auth/client'
{=/ isKeycloakAuthEnabled =}
{=# isGitHubAuthEnabled =}
export { gitHubSignInUrl, GitHubSignInButton } from '@wasp.sh/auth/client'
{=/ isGitHubAuthEnabled =}
{=# isMicrosoftAuthEnabled =}
export { microsoftSignInUrl, MicrosoftSignInButton } from '@wasp.sh/auth/client'
{=/ isMicrosoftAuthEnabled =}
export {
  default as useAuth,
  getMe,
} from '../../auth/useAuth'

export { default as logout } from '../../auth/logout'

export { resumeSession, loginWithAuthProvider } from './providers'
