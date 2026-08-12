import { isValidEmail } from '@wasp.sh/lib-auth'

/**
 * Deliberately not `type="email"`: browsers validate that against the HTML5
 * grammar, which is ASCII-only and would reject internationalized addresses
 * that the server accepts. `inputMode` keeps the email keyboard on mobile.
 */
export const emailInputProps = {
  type: 'text',
  inputMode: 'email',
  autoComplete: 'email',
} as const

/**
 * react-hook-form rules that mirror the server side `ensureValidEmail`.
 */
export const emailFieldRules = {
  required: 'Email is required',
  validate: (email: string) =>
    isValidEmail(email) || 'Email must be a valid email',
}
