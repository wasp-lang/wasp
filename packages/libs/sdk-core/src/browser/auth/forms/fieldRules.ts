import { isValidEmail } from "@wasp/lib-auth";
import {
  getFirstErrorMessage,
  type PasswordValidationSchema,
} from "../../../auth/validation.js";

export const emailFieldRules = {
  required: "Email is required",
  setValueAs: (email: string) => email.trim(),
  validate: (email: string) =>
    isValidEmail(email) || "Email must be a valid email",
};

export const usernameFieldRules = {
  required: "Username is required",
};

export const passwordFieldRules = {
  required: "Password is required",
};

export function newPasswordFieldRules(schema: PasswordValidationSchema) {
  return {
    ...passwordFieldRules,
    validate: (password: string) => validatePassword(password, schema),
  };
}

function validatePassword(
  password: string,
  schema: PasswordValidationSchema,
): string | undefined {
  const result = schema.safeParse(password);
  return result.error && getFirstErrorMessage(result.error);
}
