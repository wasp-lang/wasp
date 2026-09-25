import { isValidEmail } from "@wasp.sh/lib-auth";
import { HttpError } from "../HttpError.js";

export const PASSWORD_FIELD = "password";
const USERNAME_FIELD = "username";
const EMAIL_FIELD = "email";
const TOKEN_FIELD = "token";

export function ensureValidEmail(args: object): void {
  validate(args, [
    {
      validates: EMAIL_FIELD,
      message: "email must be present",
      validator: (email) => !!email,
    },
    {
      validates: EMAIL_FIELD,
      message: "email must be a valid email",
      validator: (email) => isValidEmail(email),
    },
  ]);
}

export function ensureValidUsername(args: object): void {
  validate(args, [
    {
      validates: USERNAME_FIELD,
      message: "username must be present",
      validator: (username) => !!username,
    },
  ]);
}

export function ensurePasswordIsPresent(args: object): void {
  validate(args, [
    {
      validates: PASSWORD_FIELD,
      message: "password must be present",
      validator: (password) => !!password,
    },
  ]);
}

export function ensureValidPassword(args: object): void {
  validate(args, [
    {
      validates: PASSWORD_FIELD,
      message: "password must be at least 8 characters",
      validator: (password) => isMinLength(password, 8),
    },
    {
      validates: PASSWORD_FIELD,
      message: "password must contain a number",
      validator: (password) => containsNumber(password),
    },
  ]);
}

export function ensureTokenIsPresent(args: object): void {
  validate(args, [
    {
      validates: TOKEN_FIELD,
      message: "token must be present",
      validator: (token) => !!token,
    },
  ]);
}

export function throwValidationError(message: string): void {
  throw new HttpError(422, "Validation failed", { message });
}

function validate(
  args: object,
  validators: {
    validates: string;
    message: string;
    validator: (value: unknown) => boolean;
  }[],
): void {
  for (const { validates, message, validator } of validators) {
    if (!validator((args as Record<string, unknown>)[validates])) {
      throwValidationError(message);
    }
  }
}

function isMinLength(input: unknown, minLength: number): boolean {
  if (typeof input !== "string") {
    return false;
  }

  return input.length >= minLength;
}

function containsNumber(input: unknown): boolean {
  if (typeof input !== "string") {
    return false;
  }

  return /\d/.test(input);
}
