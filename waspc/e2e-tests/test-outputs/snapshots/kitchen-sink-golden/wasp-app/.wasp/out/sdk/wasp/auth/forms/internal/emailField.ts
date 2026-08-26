import { isValidEmail } from '@wasp.sh/lib-auth'

/**
 * Deliberately avoids setting `type="email"`.
 * Browsers validate email input against the HTML5 grammar, which is ASCII-only
 * and would reject internationalized addresses that the server accepts.
 * `inputMode` keeps the email keyboard on mobile.
 *
 * @see {@link https://github.com/whatwg/html/issues/4562 WHATWG international email addresses issue}
 */
export const emailInputProps = {
  type: 'text',
  inputMode: 'email',
  autoComplete: 'email',
} as const

export const emailFieldRules = {
  required: 'Email is required',
  // `type="email"` used to strip surrounding whitespace for us, `type="text"` doesn't.
  setValueAs: (email: string) => email.trim(),
  validate: (email: string) =>
    isValidEmail(email) || 'Email must be a valid email',
}
