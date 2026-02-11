{{={= =}=}}
// PUBLIC API
export { LoginForm } from '../../auth/forms/Login'
export { SignupForm } from '../../auth/forms/Signup'
{=# isEmailAuthEnabled =}
export { ForgotPasswordForm } from '../../auth/forms/ForgotPassword'
export { VerifyEmailForm } from '../../auth/forms/VerifyEmail'
export { ResetPasswordForm } from '../../auth/forms/ResetPassword'
{=/ isEmailAuthEnabled =}
export type { CustomizationOptions } from '../../../core/auth/forms/types'
{=# isSlackAuthEnabled =}
export { SignInButton as SlackSignInButton } from '../../../core/auth/helpers/Slack'
{=/ isSlackAuthEnabled =}
{=# isDiscordAuthEnabled =}
export { SignInButton as DiscordSignInButton } from '../../../core/auth/helpers/Discord'
{=/ isDiscordAuthEnabled =}
{=# isGoogleAuthEnabled =}
export { SignInButton as GoogleSignInButton } from '../../../core/auth/helpers/Google'
{=/ isGoogleAuthEnabled =}
{=# isKeycloakAuthEnabled =}
export { SignInButton as KeycloakSignInButton } from '../../../core/auth/helpers/Keycloak'
{=/ isKeycloakAuthEnabled =}
{=# isGitHubAuthEnabled =}
export { SignInButton as GitHubSignInButton } from '../../../core/auth/helpers/GitHub'
{=/ isGitHubAuthEnabled =}
export {
  FormError,
  FormInput,
  FormTextarea,
  FormItemGroup,
  FormLabel,
} from '../../../core/auth/forms/internal/Form'
