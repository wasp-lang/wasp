import type { Request } from "express";

import {
  DEV_MAILBOX_SEARCH_FIELDS,
  type DevMailboxEmailFilter,
  type DevMailboxSearchField,
} from "../core/emailFilter.js";
import { ApiError } from "./apiError.js";

const DEFAULT_WAIT_TIMEOUT_MS = 10_000;
const MAX_WAIT_TIMEOUT_MS = 5 * 60_000;

type RequestInput = Record<string, unknown>;

export function getJsonBody(req: Request): RequestInput {
  if (req.body === undefined) {
    throw new Error(
      "The dev mailbox router must parse JSON bodies, e.g. with `express.json()`.",
    );
  }
  return typeof req.body === "object" && req.body !== null ? req.body : {};
}

export function parseFilter(input: RequestInput): DevMailboxEmailFilter {
  const filter: DevMailboxEmailFilter = {};
  for (const key of ["after", "search"] as const) {
    const value = input[key];
    if (value === undefined || value === "") {
      continue;
    }
    if (typeof value !== "string") {
      throw invalidFilter(`\`${key}\` must be a string.`);
    }
    filter[key] = value;
  }
  const searchIn = parseSearchIn(input.searchIn);
  if (searchIn !== undefined) {
    filter.searchIn = searchIn;
  }
  return filter;
}

export function parseLimit(value: unknown): number | undefined {
  const limit = parseOptionalPositiveInteger(value);
  if (limit === null) {
    throw new ApiError(
      400,
      "invalid_limit",
      `\`limit\` must be a positive integer, got ${JSON.stringify(value)}.`,
    );
  }
  return limit;
}

export function parseTimeoutMs(value: unknown): number {
  const timeoutMs = parseOptionalPositiveInteger(value);
  if (timeoutMs === null || (timeoutMs ?? 0) > MAX_WAIT_TIMEOUT_MS) {
    throw new ApiError(
      400,
      "invalid_timeout",
      `\`timeoutMs\` must be a positive integer of at most ${MAX_WAIT_TIMEOUT_MS}, got ${JSON.stringify(value)}.`,
    );
  }
  return timeoutMs ?? DEFAULT_WAIT_TIMEOUT_MS;
}

export function parseEmailId(value: unknown): string {
  if (typeof value !== "string") {
    throw new ApiError(400, "invalid_id", "`id` must be a string.");
  }
  return value;
}

/** An array, or a comma-separated string in query parameters. */
function parseSearchIn(value: unknown): DevMailboxSearchField[] | undefined {
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
    throw invalidFilter(
      `\`searchIn\` must list some of: ${DEV_MAILBOX_SEARCH_FIELDS.join(", ")}.`,
    );
  }
  return fields as DevMailboxSearchField[];
}

/** `undefined` when missing, `null` when invalid. */
function parseOptionalPositiveInteger(
  value: unknown,
): number | undefined | null {
  if (value === undefined || value === "") {
    return undefined;
  }
  const number = typeof value === "string" ? Number(value) : value;
  return typeof number === "number" && Number.isInteger(number) && number >= 1
    ? number
    : null;
}

function invalidFilter(message: string): ApiError {
  return new ApiError(400, "invalid_filter", message);
}
