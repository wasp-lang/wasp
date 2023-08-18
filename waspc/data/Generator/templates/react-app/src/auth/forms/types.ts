{{={= =}=}}
import { createTheme } from '@stitches/react'
import { useForm } from 'react-hook-form'
import type { LoginSignupFormFields } from './internal/common/LoginSignupForm'

export enum State {
  Login = 'login',
  Signup = 'signup',
  {=# isEmailAuthEnabled =}
  ForgotPassword = 'forgot-password',
  ResetPassword = 'reset-password',
  VerifyEmail = 'verify-email',
  {=/ isEmailAuthEnabled =}
}

export type CustomizationOptions = {
  logo?: string
  socialLayout?: 'horizontal' | 'vertical'
  appearance?: Parameters<typeof createTheme>[0]
}

export type ErrorMessage = {
  title: string
  description?: string
}

export type AdditionalSignupFields = (
  hookForm: ReturnType<typeof useForm<LoginSignupFormFields>>
) => React.ReactNode;
