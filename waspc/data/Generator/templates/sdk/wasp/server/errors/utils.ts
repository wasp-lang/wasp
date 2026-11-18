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
 * The header values Node's `response.setHeader` accepts, which is what errors
 * from Express middleware carry.
 */
export type NodeResponseHeaders = {
  [headerName: string]: string | number | string[];
};

// PRIVATE API (server)
export function toHeadersInit(
  headers: NodeResponseHeaders | undefined,
): [string, string][] {
  return Object.entries(headers ?? {}).flatMap(([name, value]) =>
    Array.isArray(value)
      ? value.map((item): [string, string] => [name, item])
      : [[name, String(value)]],
  );
}
