{{={= =}=}}
import { EmailFromField } from "./types";

// Formats an email address and an optional name into a string that can be used
// as the "from" field in an email.
// { email: "test@test.com, name: "Test" } -> "Test <test@test.com>"
export function formatFromField({
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
