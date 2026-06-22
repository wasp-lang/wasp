import {
  ensurePasswordIsPresent,
  ensureValidPassword,
  ensureValidUsername,
} from "../../validation";

type UsernameCredentials = {
  username: string;
  password: string;
};

export function getUsernameSignupCredentials(
  fields: object,
): UsernameCredentials {
  ensureValidUsername(fields);
  ensurePasswordIsPresent(fields);
  ensureValidPassword(fields);
  return fields as UsernameCredentials;
}

export function getUsernameLoginCredentials(
  fields: object,
): UsernameCredentials {
  ensureValidUsername(fields);
  ensurePasswordIsPresent(fields);
  return fields as UsernameCredentials;
}
