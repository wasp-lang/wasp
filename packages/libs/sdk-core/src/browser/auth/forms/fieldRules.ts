import { isValidEmail } from "@wasp/lib-auth";

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
