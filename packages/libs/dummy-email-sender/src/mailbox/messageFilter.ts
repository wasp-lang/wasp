import type { DevMailboxMessage } from "./message.js";

export type DevMailboxMessageFilter = {
  /** A message id. Only messages sent after it match. */
  after?: string;
  /**
   * Text one of the `searchIn` fields contains, matched case-insensitively.
   */
  search?: string;
  /** Fields `search` looks in. Defaults to all of them. */
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
  message: DevMailboxMessage,
  filter: DevMailboxMessageFilter,
): boolean {
  if (filter.after !== undefined && message.id <= filter.after) {
    return false;
  }
  if (
    filter.search !== undefined &&
    !matchesSearch(message, filter.search, filter.searchIn)
  ) {
    return false;
  }
  return true;
}

function matchesSearch(
  message: DevMailboxMessage,
  search: string,
  searchIn: readonly DevMailboxSearchField[] = DEV_MAILBOX_SEARCH_FIELDS,
): boolean {
  return searchIn.some((field) =>
    containsIgnoringCase(getSearchableText(message, field), search),
  );
}

function getSearchableText(
  message: DevMailboxMessage,
  field: DevMailboxSearchField,
): string {
  switch (field) {
    case "from":
      return `${message.from.name ?? ""} ${message.from.email}`;
    case "to":
      return message.to;
    case "subject":
      return message.subject;
    case "body":
      // Tags and attributes aren't part of what the reader sees.
      return `${message.text} ${message.html.replace(/<[^>]*>/g, " ")}`;
  }
}

function containsIgnoringCase(text: string, part: string): boolean {
  return text.toLowerCase().includes(part.toLowerCase());
}
