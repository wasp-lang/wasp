import { createTheme } from '@stitches/react'
import { UseFormReturn, RegisterOptions } from 'react-hook-form'
import type { LoginSignupFormFields } from './internal/common/LoginSignupForm'

// PRIVATE API
export enum State {
  Login = 'login',
  Signup = 'signup',
  ForgotPassword = 'forgot-password',
  ResetPassword = 'reset-password',
  VerifyEmail = 'verify-email',
}

// PUBLIC API
export type CustomizationOptions = {
  logo?: string
  socialLayout?: 'horizontal' | 'vertical'
  appearance?: Parameters<typeof createTheme>[0]
}

// PRIVATE API
export type ErrorMessage = {
  title: string
  description?: string
}

// PRIVATE API
export type FormState = {
  isLoading: boolean
}

// PRIVATE API
export type AdditionalSignupFieldRenderFn = (
  hookForm: UseFormReturn<LoginSignupFormFields>,
  formState: FormState
) => React.ReactNode

// PRIVATE API
export type AdditionalSignupField = {
  name: string
  label: string
  type: 'input' | 'textarea'
  validations?: RegisterOptions<LoginSignupFormFields>
}

// PRIVATE API
export type AdditionalSignupFields =
  | (AdditionalSignupField | AdditionalSignupFieldRenderFn)[]
  | AdditionalSignupFieldRenderFn
