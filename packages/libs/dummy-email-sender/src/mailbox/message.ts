import type { EmailFromField } from "../types.js";

/** An email as the Dummy email sender hands it to the dev mailbox. */
export type DevMailboxEmail = {
  from: EmailFromField;
  to: string;
  subject: string;
  text: string;
  html: string;
};

/**
 * An email stored in the dev mailbox. This is also the shape of the files in
 * `.wasp/mailbox/` and of the API responses, so tests and agents rely on it.
 */
export type DevMailboxMessage = DevMailboxEmail & {
  id: string;
  /** ISO 8601 timestamp. */
  sentAt: string;
  /** URLs found in the text and HTML bodies, in order of appearance. */
  links: string[];
};

export type DevMailboxMessageSummary = Pick<
  DevMailboxMessage,
  "id" | "sentAt" | "from" | "to" | "subject"
>;

export function createMessage(
  email: DevMailboxEmail,
  id: string,
  sentAt: Date,
): DevMailboxMessage {
  return {
    id,
    sentAt: sentAt.toISOString(),
    from: email.from,
    to: email.to,
    subject: email.subject,
    text: email.text,
    html: email.html,
    links: extractLinks(email),
  };
}

export function summarizeMessage(
  message: DevMailboxMessage,
): DevMailboxMessageSummary {
  return {
    id: message.id,
    sentAt: message.sentAt,
    from: message.from,
    to: message.to,
    subject: message.subject,
  };
}

const HREF_PATTERN = /href\s*=\s*(?:"([^"]*)"|'([^']*)')/gi;
const URL_PATTERN = /https?:\/\/[^\s<>"']+/gi;
const TRAILING_PUNCTUATION_PATTERN = /[.,;:!?)\]]+$/;

export function extractLinks(email: Pick<DevMailboxEmail, "text" | "html">) {
  const hrefLinks = [...email.html.matchAll(HREF_PATTERN)]
    .map((match) => decodeHtmlEntities(match[1] ?? match[2] ?? ""))
    .filter(isHttpUrl);
  const textLinks = [...email.text.matchAll(URL_PATTERN)].map((match) =>
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
