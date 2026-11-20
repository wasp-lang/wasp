{{={= =}=}}
import {
  defaultPasswordValidationSchema,
  ensureValidPassword as ensureValidPasswordForSchema,
  type PasswordValidationSchema,
} from '@wasp/lib-sdk-core'
import type { FromRegister } from '../types/register'
{=# passwordValidationSchema.isDefined =}
{=& passwordValidationSchema.importStatement =}
{=/ passwordValidationSchema.isDefined =}

export { ensurePasswordIsPresent, ensureTokenIsPresent, ensureValidEmail, ensureValidUsername, PASSWORD_FIELD, throwValidationError } from '@wasp/lib-sdk-core'

export type RegisteredPasswordValidationSchema = FromRegister<"passwordValidationSchema", PasswordValidationSchema>

// PUBLIC API
{=# passwordValidationSchema.isDefined =}
export const passwordValidationSchema: RegisteredPasswordValidationSchema = {= passwordValidationSchema.importIdentifier =}
{=/ passwordValidationSchema.isDefined =}
{=^ passwordValidationSchema.isDefined =}
export const passwordValidationSchema: PasswordValidationSchema = defaultPasswordValidationSchema
{=/ passwordValidationSchema.isDefined =}

// PUBLIC API
export function ensureValidPassword(args: object): void {
  ensureValidPasswordForSchema(args, passwordValidationSchema)
}
