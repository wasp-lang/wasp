import type { DevMailboxEmail } from "./email.js";

export type DevMailboxEmailFilter = {
  /** Only emails sent after the one with this id. */
  after?: string;
  /** Matched case-insensitively. */
  search?: string;
  /** Defaults to all fields. */
  searchIn?: DevMailboxSearchField[];
};

export const DEV_MAILBOX_SEARCH_FIELDS = [
  "from",
  "to",
  "subject",
  "body",
] as const;

export type DevMailboxSearchField = (typeof DEV_MAILBOX_SEARCH_FIELDS)[number];

export function matchesFilter(
  email: DevMailboxEmail,
  filter: DevMailboxEmailFilter,
): boolean {
  if (filter.after !== undefined && email.id <= filter.after) {
    return false;
  }
  if (
    filter.search !== undefined &&
    !matchesSearch(email, filter.search, filter.searchIn)
  ) {
    return false;
  }
  return true;
}

function matchesSearch(
  email: DevMailboxEmail,
  search: string,
  searchIn: readonly DevMailboxSearchField[] = DEV_MAILBOX_SEARCH_FIELDS,
): boolean {
  return searchIn.some((field) =>
    containsIgnoringCase(getSearchableText(email, field), search),
  );
}

function getSearchableText(
  email: DevMailboxEmail,
  field: DevMailboxSearchField,
): string {
  switch (field) {
    case "from":
      return `${email.from.name ?? ""} ${email.from.email}`;
    case "to":
      return email.to;
    case "subject":
      return email.subject;
    case "body":
      return `${email.text} ${stripHtmlTags(email.html)}`;
  }
}

function stripHtmlTags(html: string): string {
  return html.replace(/<[^>]*>/g, " ");
}

function containsIgnoringCase(text: string, part: string): boolean {
  return text.toLowerCase().includes(part.toLowerCase());
}
