import {
  ensurePasswordIsPresent,
  ensureTokenIsPresent,
  ensureValidEmail,
  ensureValidPassword,
} from "../../validation";

type EmailCredentials = {
  email: string;
  password: string;
};

type EmailFields = {
  email: string;
};

type TokenFields = {
  token: string;
};

export function getEmailSignupCredentials(fields: object): EmailCredentials {
  ensureValidEmail(fields);
  ensurePasswordIsPresent(fields);
  ensureValidPassword(fields);
  return fields as EmailCredentials;
}

export function getEmailLoginCredentials(fields: object): EmailCredentials {
  ensureValidEmail(fields);
  ensurePasswordIsPresent(fields);
  return fields as EmailCredentials;
}

export function getPasswordResetRequestFields(fields: object): EmailFields {
  ensureValidEmail(fields);
  return fields as EmailFields;
}

export function getPasswordResetFields(
  fields: object,
): EmailCredentials & TokenFields {
  ensureTokenIsPresent(fields);
  ensurePasswordIsPresent(fields);
  ensureValidPassword(fields);
  return fields as EmailCredentials & TokenFields;
}

export function getEmailVerificationFields(fields: object): TokenFields {
  return fields as TokenFields;
}
