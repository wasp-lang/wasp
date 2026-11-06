import { isValidEmail } from "@wasp/lib-auth";
import * as z from "zod";
import { HttpError } from "../HttpError.js";

export const PASSWORD_FIELD = "password";
const USERNAME_FIELD = "username";
const EMAIL_FIELD = "email";
const TOKEN_FIELD = "token";

export function ensureValidEmail(args: object): void {
  const email = EMAIL_FIELD in args ? args[EMAIL_FIELD] : undefined;
  validate(email, emailSchema);
}

export function ensureValidUsername(args: object): void {
  const username = USERNAME_FIELD in args ? args[USERNAME_FIELD] : undefined;
  validate(username, usernameSchema);
}

export function ensurePasswordIsPresent(args: object): void {
  const password = PASSWORD_FIELD in args ? args[PASSWORD_FIELD] : undefined;
  validate(password, presentPasswordSchema);
}

export function ensureValidPassword(args: object): void {
  const password = PASSWORD_FIELD in args ? args[PASSWORD_FIELD] : undefined;
  validate(password, passwordSchema);
}

export function ensureTokenIsPresent(args: object): void {
  const token = TOKEN_FIELD in args ? args[TOKEN_FIELD] : undefined;
  validate(token, tokenSchema);
}

export function throwValidationError(message: string): void {
  throw new HttpError(422, "Validation failed", { message });
}

const PASSWORD_MIN_LENGTH_MESSAGE = "password must be at least 8 characters";

const emailSchema = presentValueSchema("email must be present").refine(
  isValidEmail,
  "email must be a valid email",
);
const usernameSchema = presentValueSchema("username must be present");
const presentPasswordSchema = presentValueSchema("password must be present");
const passwordSchema = z
  .string(PASSWORD_MIN_LENGTH_MESSAGE)
  .min(8, PASSWORD_MIN_LENGTH_MESSAGE)
  .regex(/\d/, "password must contain a number");
const tokenSchema = presentValueSchema("token must be present");

function presentValueSchema(message: string): z.ZodType {
  return z
    .unknown()
    .refine((value) => !!value, { error: message, abort: true });
}

function validate(value: unknown, schema: z.ZodType): void {
  const result = schema.safeParse(value);
  if (!result.success) {
    throwValidationError(getFirstErrorMessage(result.error));
  }
}

// A failed parse always has at least one issue.
function getFirstErrorMessage(error: z.ZodError): string {
  return error.issues[0]!.message;
}
