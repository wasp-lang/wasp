import { RegisterOptions, UseFormReturn } from 'react-hook-form'
import { FormState } from '../../../core/auth/forms/types'
import type { LoginSignupFormFields } from './internal/common/LoginSignupForm'

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
