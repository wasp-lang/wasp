import {
  ensurePasswordIsPresent,
  ensureValidEmail,
  ensureValidPassword,
} from "../../validation";

type EmailCredentials = {
  email: string;
  password: string;
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
