import type { EmailFromField } from "../emailFromField.js";
import { generateEmailId } from "./emailId.js";

export type NewDevMailboxEmail = {
  from: EmailFromField;
  to: string;
  subject: string;
  text: string;
  html: string;
};

/** Also the shape of the stored files and the API responses. */
export type DevMailboxEmail = NewDevMailboxEmail & {
  id: string;
  /** ISO 8601 timestamp. */
  sentAt: string;
  /** URLs in the text and HTML bodies, in order. */
  links: string[];
};

export type DevMailboxEmailSummary = Pick<
  DevMailboxEmail,
  "id" | "sentAt" | "from" | "to" | "subject"
>;

export function createEmail(
  newEmail: NewDevMailboxEmail,
  sentAt: Date,
): DevMailboxEmail {
  return {
    id: generateEmailId(sentAt),
    sentAt: sentAt.toISOString(),
    from: newEmail.from,
    to: newEmail.to,
    subject: newEmail.subject,
    text: newEmail.text,
    html: newEmail.html,
    links: extractLinks(newEmail),
  };
}

export function summarizeEmail(email: DevMailboxEmail): DevMailboxEmailSummary {
  return {
    id: email.id,
    sentAt: email.sentAt,
    from: email.from,
    to: email.to,
    subject: email.subject,
  };
}

/** Guards against hand-edited or truncated files. */
export function isDevMailboxEmail(value: unknown): value is DevMailboxEmail {
  if (typeof value !== "object" || value === null) {
    return false;
  }
  const email = value as Record<string, unknown>;
  const from = email.from as Record<string, unknown> | null | undefined;
  return (
    ["id", "sentAt", "to", "subject", "text", "html"].every(
      (key) => typeof email[key] === "string",
    ) &&
    typeof from === "object" &&
    from !== null &&
    typeof from.email === "string" &&
    Array.isArray(email.links) &&
    email.links.every((link) => typeof link === "string")
  );
}

const HREF_PATTERN = /href\s*=\s*(?:"([^"]*)"|'([^']*)')/gi;
const URL_PATTERN = /https?:\/\/[^\s<>"']+/gi;
const TRAILING_PUNCTUATION_PATTERN = /[.,;:!?)\]]+$/;

export function extractLinks(
  body: Pick<NewDevMailboxEmail, "text" | "html">,
): string[] {
  const hrefLinks = [...body.html.matchAll(HREF_PATTERN)]
    .map((match) => decodeHtmlEntities(match[1] ?? match[2] ?? ""))
    .filter(isHttpUrl);
  const textLinks = [...body.text.matchAll(URL_PATTERN)].map((match) =>
    match[0].replace(TRAILING_PUNCTUATION_PATTERN, ""),
  );
  return [...new Set([...textLinks, ...hrefLinks])];
}

function isHttpUrl(value: string): boolean {
  return /^https?:\/\//i.test(value);
}

function decodeHtmlEntities(value: string): string {
  return value
    .replace(/&quot;/g, '"')
    .replace(/&#39;/g, "'")
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/&amp;/g, "&");
}
