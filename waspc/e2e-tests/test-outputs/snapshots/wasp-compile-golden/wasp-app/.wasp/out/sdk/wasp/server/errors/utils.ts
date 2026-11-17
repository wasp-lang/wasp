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
