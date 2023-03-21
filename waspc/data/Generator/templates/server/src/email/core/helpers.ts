{{={= =}=}}
import { EmailFromField } from "./types";

export function createFromEmailString({
  email,
  name,
}: {
  email: string;
  name?: string;
}): string {
  if (name) {
    return `${name} <${email}>`;
  }
  return email;
}

export function getDefaultFromField(): EmailFromField {
  return {
    email: "{= senderDefaults.email =}",
    {=# senderDefaults.isNameDefined =}
    name: "{= senderDefaults.name =}",
    {=/ senderDefaults.isNameDefined =}
  }
}
