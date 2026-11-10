import { isValidEmail } from "@wasp/lib-auth";
import * as z from "zod";
import { HttpError } from "../HttpError.js";

export type PasswordValidationSchema = z.ZodType<string, string>;

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

export function ensureValidPassword(
  args: object,
  schema: PasswordValidationSchema = defaultPasswordValidationSchema,
): void {
  const password = PASSWORD_FIELD in args ? args[PASSWORD_FIELD] : undefined;
  validate(password, schema);
}

export function definePasswordValidationSchema<
  Schema extends PasswordValidationSchema,
>(schema: Schema): Schema {
  return schema;
}

export function ensureTokenIsPresent(args: object): void {
  const token = TOKEN_FIELD in args ? args[TOKEN_FIELD] : undefined;
  validate(token, tokenSchema);
}

export function throwValidationError(message: string): void {
  throw new HttpError(422, "Validation failed", { message });
}

const PASSWORD_MIN_LENGTH_MESSAGE = "Password must be at least 8 characters";

const emailSchema = presentValueSchema("Email must be present").refine(
  isValidEmail,
  "Email must be a valid email",
);
const usernameSchema = presentValueSchema("Username must be present");
const presentPasswordSchema = presentValueSchema("Password must be present");
export const defaultPasswordValidationSchema = z
  .string(PASSWORD_MIN_LENGTH_MESSAGE)
  .min(8, PASSWORD_MIN_LENGTH_MESSAGE)
  .regex(/\d/, "Password must contain a number");
const tokenSchema = presentValueSchema("Token must be present");

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

export function getFirstErrorMessage(error: z.ZodError): string {
  // A failed parse always has at least one issue.
  return error.issues[0]!.message;
}
