import type { Request } from "express";

import {
  DEV_MAILBOX_SEARCH_FIELDS,
  type DevMailboxMessageFilter,
  type DevMailboxSearchField,
} from "../core/messageFilter.js";

const DEFAULT_WAIT_TIMEOUT_MS = 10_000;
const MAX_WAIT_TIMEOUT_MS = 5 * 60_000;

export class InvalidInputError extends Error {}

type Input = Record<string, unknown>;

export function getBody(req: Request): Input {
  return typeof req.body === "object" && req.body !== null ? req.body : {};
}

export function parseFilter(
  input: Input,
): DevMailboxMessageFilter | InvalidInputError {
  const filter: DevMailboxMessageFilter = {};
  for (const key of ["after", "search"] as const) {
    const value = input[key];
    if (value === undefined || value === "") {
      continue;
    }
    if (typeof value !== "string") {
      return new InvalidInputError(`\`${key}\` must be a string.`);
    }
    filter[key] = value;
  }

  const searchIn = parseSearchIn(input.searchIn);
  if (searchIn instanceof InvalidInputError) {
    return searchIn;
  }
  if (searchIn !== undefined) {
    filter.searchIn = searchIn;
  }
  return filter;
}

/** An array, or a comma-separated string in query parameters. */
function parseSearchIn(
  value: unknown,
): DevMailboxSearchField[] | undefined | InvalidInputError {
  if (value === undefined || value === "") {
    return undefined;
  }
  const fields = typeof value === "string" ? value.split(",") : value;
  const isValid =
    Array.isArray(fields) &&
    fields.length > 0 &&
    fields.every((field) =>
      (DEV_MAILBOX_SEARCH_FIELDS as readonly unknown[]).includes(field),
    );
  if (!isValid) {
    return new InvalidInputError(
      `\`searchIn\` must list some of: ${DEV_MAILBOX_SEARCH_FIELDS.join(", ")}.`,
    );
  }
  return fields as DevMailboxSearchField[];
}

export function parseOptionalPositiveInteger(
  value: unknown,
): number | undefined | InvalidInputError {
  if (value === undefined || value === "") {
    return undefined;
  }
  const number = typeof value === "string" ? Number(value) : value;
  if (typeof number !== "number" || !Number.isInteger(number) || number < 1) {
    return new InvalidInputError(
      `Expected a positive integer, got ${JSON.stringify(value)}.`,
    );
  }
  return number;
}

export function parseTimeoutMs(value: unknown): number | InvalidInputError {
  const timeoutMs = parseOptionalPositiveInteger(value);
  if (timeoutMs instanceof InvalidInputError || timeoutMs === undefined) {
    return timeoutMs ?? DEFAULT_WAIT_TIMEOUT_MS;
  }
  if (timeoutMs > MAX_WAIT_TIMEOUT_MS) {
    return new InvalidInputError(
      `\`timeoutMs\` must be at most ${MAX_WAIT_TIMEOUT_MS}, got ${timeoutMs}.`,
    );
  }
  return timeoutMs;
}
