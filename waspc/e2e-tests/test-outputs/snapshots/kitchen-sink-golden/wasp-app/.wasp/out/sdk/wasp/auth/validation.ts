import {
  defaultPasswordValidationSchema,
  ensureValidPassword as ensureValidPasswordForSchema,
  type PasswordValidationSchema,
} from '@wasp/lib-sdk-core'
import type { FromRegister } from '../types/register'
import { passwordValidationSchema as passwordValidationSchema_ext } from "virtual:wasp/user/features/auth/password"

export { ensurePasswordIsPresent, ensureTokenIsPresent, ensureValidEmail, ensureValidUsername, PASSWORD_FIELD, throwValidationError } from '@wasp/lib-sdk-core'

export type RegisteredPasswordValidationSchema = FromRegister<"passwordValidationSchema", PasswordValidationSchema>

// PUBLIC API
export const passwordValidationSchema: RegisteredPasswordValidationSchema = passwordValidationSchema_ext

// PUBLIC API
export function ensureValidPassword(args: object): void {
  ensureValidPasswordForSchema(args, passwordValidationSchema)
}
