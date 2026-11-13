// Wasp adapter for DomainError: operations throw HttpError so the dashboard
// gets the right status code and message, and MCP the right tool error.
import { HttpError } from "wasp/server";
import { toErrorResponse } from "./errors";

export function toWaspHttpError(e: unknown): never {
  if (e instanceof HttpError) throw e;
  const { status, body } = toErrorResponse(e);
  if (status === 500) {
    console.error(e);
    throw new HttpError(500, body.error.message);
  }
  throw new HttpError(status, body.error.message, {
    code: body.error.code,
    details: body.error.details,
  });
}

/** Runs a service call and converts DomainErrors for the dashboard. */
export async function runOperation<T>(fn: () => Promise<T>): Promise<T> {
  try {
    return await fn();
  } catch (e) {
    toWaspHttpError(e);
  }
}
