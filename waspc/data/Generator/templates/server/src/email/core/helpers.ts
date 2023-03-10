{{={= =}=}}

import { EmailFromField } from "./types";

export function createFromEmailString({
  email,
  title,
}: {
  email: string;
  title?: string;
}): string {
  if (title) {
    return `${title} <${email}>`;
  }
  return email;
}

export function getDefaultFromField(): EmailFromField {
  return {
    email: "{= senderDefaults.email =}",
    {=# senderDefaults.isTitleDefined =}
    title: "{= senderDefaults.title =}",
    {=/ senderDefaults.isTitleDefined =}
  }
}
