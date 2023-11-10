import HttpError from '../core/HttpError.js';
import { isValidEmail } from '../core/auth/validators.js'

export const PASSWORD_FIELD = 'password';
const USERNAME_FIELD = 'username';
const EMAIL_FIELD = 'email';
const TOKEN_FIELD = 'token';

const passwordValidators = [
  { validates: PASSWORD_FIELD, message: 'password must be present', validator: password => !!password },
  { validates: PASSWORD_FIELD, message: 'password must be at least 8 characters', validator: password => password.length >= 8 },
  { validates: PASSWORD_FIELD, message: 'password must contain a number', validator: password => /\d/.test(password) },
];
const usernameValidators = [
  { validates: USERNAME_FIELD, message: 'username must be present', validator: username => !!username }
];
const emailValidators = [
  { validates: EMAIL_FIELD, message: 'email must be present', validator: email => !!email },
  { validates: EMAIL_FIELD, message: 'email must be a valid email', validator: email => isValidEmail(email) },
];
const tokenValidators = [
  { validates: TOKEN_FIELD, message: 'token must be present', validator: token => !!token },
];

export function ensureValidEmailAndPassword(args: unknown): void {
  ensureValidEmail(args);
  ensureValidPassword(args);
}

export function ensureValidEmail(args: unknown): void {
  validate(args, [
    ...emailValidators,
  ]);
}

export function ensureValidUsernameAndPassword(args: unknown): void {
  ensureValidUsername(args);
  ensureValidPassword(args);
}

export function ensureValidTokenAndNewPassword(args: unknown): void {
  validate(args, [
    ...tokenValidators,
  ]);
  ensureValidPassword(args);
}

export function ensureValidUsername(args: unknown): void {
  validate(args, [
    ...usernameValidators,
  ]);
}

export function ensureValidPassword(args: unknown): void {
  validate(args, [
    ...passwordValidators,
  ]);
}

export function throwValidationError(message: string): void {
  throw new HttpError(422, 'Validation failed', { message })
}

function validate(args: unknown, validators: { validates: string, message: string, validator: (value: unknown) => boolean }[]): void {
  for (const { validates, message, validator } of validators) {
    if (!validator(args[validates])) {
      throwValidationError(message);
    }
  }
}
