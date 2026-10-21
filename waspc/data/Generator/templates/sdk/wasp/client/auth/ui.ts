{{={= =}=}}
// PUBLIC API
export { LoginForm, SignupForm } from '../../auth/forms/index.js'
{=# isEmailAuthEnabled =}
export { ForgotPasswordForm, VerifyEmailForm, ResetPasswordForm } from '../../auth/forms/index.js'
{=/ isEmailAuthEnabled =}
export type { CustomizationOptions } from '@wasp.sh/lib-auth/browser'
{=# isSlackAuthEnabled =}
export { SignInButton as SlackSignInButton } from '../../auth/helpers/Slack'
{=/ isSlackAuthEnabled =}
{=# isDiscordAuthEnabled =}
export { SignInButton as DiscordSignInButton } from '../../auth/helpers/Discord'
{=/ isDiscordAuthEnabled =}
{=# isGoogleAuthEnabled =}
export { SignInButton as GoogleSignInButton } from '../../auth/helpers/Google'
{=/ isGoogleAuthEnabled =}
{=# isKeycloakAuthEnabled =}
export { SignInButton as KeycloakSignInButton } from '../../auth/helpers/Keycloak'
{=/ isKeycloakAuthEnabled =}
{=# isGitHubAuthEnabled =}
export { SignInButton as GitHubSignInButton } from '../../auth/helpers/GitHub'
{=/ isGitHubAuthEnabled =}
{=# isMicrosoftAuthEnabled =}
export { SignInButton as MicrosoftSignInButton } from '../../auth/helpers/Microsoft'
{=/ isMicrosoftAuthEnabled =}
export {
  FormError,
  FormInput,
  FormTextarea,
  FormItemGroup,
  FormLabel,
} from '@wasp.sh/lib-auth/browser'
