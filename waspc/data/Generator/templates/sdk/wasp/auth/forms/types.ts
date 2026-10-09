{{={= =}=}}

// PRIVATE API
export enum State {
  Login = 'login',
  Signup = 'signup',
  {=# isEmailAuthEnabled =}
  ForgotPassword = 'forgot-password',
  ResetPassword = 'reset-password',
  VerifyEmail = 'verify-email',
  {=/ isEmailAuthEnabled =}
}

export { type AdditionalSignupField, type AdditionalSignupFieldRenderFn, type AdditionalSignupFields, type CustomizationOptions, type FormState } from '@wasp.sh/lib-sdk-core/browser'
