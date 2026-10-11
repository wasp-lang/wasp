import { isValidEmail } from '@wasp.sh/lib-auth';
import { HttpError } from '../server/index.js';

export const PASSWORD_FIELD = 'password';
const USERNAME_FIELD = 'username';
const EMAIL_FIELD = 'email';
const TOKEN_FIELD = 'token';

// PUBLIC API
export function ensureValidEmail(args: object): void {
  const email = EMAIL_FIELD in args ? args[EMAIL_FIELD] : undefined;
  validate(email, [
    { message: 'email must be present', validator: email => !!email },
    { message: 'email must be a valid email', validator: email => isValidEmail(email) },
  ]);
}

// PUBLIC API
export function ensureValidUsername(args: object): void {
  const username = USERNAME_FIELD in args ? args[USERNAME_FIELD] : undefined;
  validate(username, [
    { message: 'username must be present', validator: username => !!username }
  ]);
}

// PUBLIC API
export function ensurePasswordIsPresent(args: object): void {
  const password = PASSWORD_FIELD in args ? args[PASSWORD_FIELD] : undefined;
  validate(password, [
    { message: 'password must be present', validator: password => !!password },
  ]);
}

// PUBLIC API
export function ensureValidPassword(args: object): void {
  const password = PASSWORD_FIELD in args ? args[PASSWORD_FIELD] : undefined;
  validate(password, [
    { message: 'password must be at least 8 characters', validator: password => isMinLength(password, 8) },
    { message: 'password must contain a number', validator: password => containsNumber(password) },
  ]);
}

// PUBLIC API
export function ensureTokenIsPresent(args: object): void {
  const token = TOKEN_FIELD in args ? args[TOKEN_FIELD] : undefined;
  validate(token, [
    { message: 'token must be present', validator: token => !!token },
  ]);
}

// PRIVATE API
export function throwValidationError(message: string): void {
  throw new HttpError(422, 'Validation failed', { message })
}

function validate(value: unknown, validators: { message: string, validator: (value: unknown) => boolean }[]): void {
  for (const { message, validator } of validators) {
    if (!validator(value)) {
      throwValidationError(message);
    }
  }
}

function isMinLength(input: unknown, minLength: number): boolean {
    if (typeof input !== 'string') {
        return false
    }

    return input.length >= minLength
}

function containsNumber(input: unknown): boolean {
    if (typeof input !== 'string') {
        return false
    }

    return /\d/.test(input)
}
