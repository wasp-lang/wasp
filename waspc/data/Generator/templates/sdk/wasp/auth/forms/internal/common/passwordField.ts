import { getFirstErrorMessage } from '@wasp/lib-sdk-core'
import { passwordValidationSchema } from '../../../validation.js'

// PRIVATE API
export const passwordFieldRules = {
  required: 'Password is required',
}

// PRIVATE API
export const newPasswordFieldRules = {
  ...passwordFieldRules,
  validate: validatePassword,
}

function validatePassword(password: unknown): string | undefined {
  const result = passwordValidationSchema.safeParse(password)
  return result.error && getFirstErrorMessage(result.error)
}
