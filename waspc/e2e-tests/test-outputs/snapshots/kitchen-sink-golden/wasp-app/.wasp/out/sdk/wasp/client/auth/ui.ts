// PUBLIC API
export { LoginForm, SignupForm } from '../../auth/forms/index.js'
export { ForgotPasswordForm, VerifyEmailForm, ResetPasswordForm } from '../../auth/forms/index.js'
export type { CustomizationOptions } from '@wasp.sh/lib-auth/browser'
export { SignInButton as SlackSignInButton } from '../../auth/helpers/Slack'
export { SignInButton as DiscordSignInButton } from '../../auth/helpers/Discord'
export { SignInButton as GoogleSignInButton } from '../../auth/helpers/Google'
export { SignInButton as GitHubSignInButton } from '../../auth/helpers/GitHub'
export { SignInButton as MicrosoftSignInButton } from '../../auth/helpers/Microsoft'
export {
  FormError,
  FormInput,
  FormTextarea,
  FormItemGroup,
  FormLabel,
} from '@wasp.sh/lib-auth/browser'
