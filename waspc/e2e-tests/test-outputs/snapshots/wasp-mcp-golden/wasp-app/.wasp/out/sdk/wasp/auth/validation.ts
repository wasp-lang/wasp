import {
  defaultPasswordValidationSchema,
  ensureValidPassword as ensureValidPasswordForSchema,
  type PasswordValidationSchema,
} from '@wasp/lib-sdk-core'
import type { FromRegister } from '../types/register'

export { ensurePasswordIsPresent, ensureTokenIsPresent, ensureValidEmail, ensureValidUsername, PASSWORD_FIELD, throwValidationError } from '@wasp/lib-sdk-core'

export type RegisteredPasswordValidationSchema = FromRegister<"passwordValidationSchema", PasswordValidationSchema>

// PUBLIC API
export const passwordValidationSchema: PasswordValidationSchema = defaultPasswordValidationSchema

// PUBLIC API
export function ensureValidPassword(args: object): void {
  ensureValidPasswordForSchema(args, passwordValidationSchema)
}
