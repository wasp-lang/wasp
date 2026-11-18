import * as Wasp from "@wasp/lib-sdk-core";

// PRIVATE API (server)
export function getErrorDetails(error: unknown): {
  message: string;
  stack: string | undefined;
} {
  return {
    message: Wasp.getErrorMessage(error),
    stack: error instanceof Error ? error.stack : undefined,
  };
}

// PRIVATE API (server)
/**
 * The `headers` of an error that isn't ours. Express sets each of them with
 * Node's `response.setHeader`, which takes a string, a number, or a list of
 * strings, but nothing checks that, so we treat the values as unknown.
 */
export type ForeignErrorHeaders = { [headerName: string]: unknown };

// PRIVATE API (server)
/**
 * Converts them to the standard form, with one pair for each value of a
 * repeated header.
 */
export function toHeadersInit(
  headers: ForeignErrorHeaders | undefined,
): [string, string][] {
  return Object.entries(headers ?? {}).flatMap(([name, value]) =>
    (Array.isArray(value) ? value : [value]).map((item): [string, string] => [
      name,
      String(item),
    ]),
  );
}
