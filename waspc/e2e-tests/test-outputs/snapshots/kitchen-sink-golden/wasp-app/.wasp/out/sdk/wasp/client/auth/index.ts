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
export { slackSignInUrl, SlackSignInButton } from '@wasp.sh/auth/client'
export { discordSignInUrl, DiscordSignInButton } from '@wasp.sh/auth/client'
export { googleSignInUrl, GoogleSignInButton } from '@wasp.sh/auth/client'
export { gitHubSignInUrl, GitHubSignInButton } from '@wasp.sh/auth/client'
export { microsoftSignInUrl, MicrosoftSignInButton } from '@wasp.sh/auth/client'
export {
  default as useAuth,
  getMe,
} from '../../auth/useAuth'

export { default as logout } from '../../auth/logout'

export { resumeSession, loginWithAuthProvider } from './providers'
