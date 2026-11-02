import * as Wasp from "@wasp/lib-sdk-core";

// PRIVATE API (server)
export function isErrorStatusCode(value: unknown): value is number {
  return (
    typeof value === "number" &&
    Number.isInteger(value) &&
    value >= 400 &&
    value < 600
  );
}

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
