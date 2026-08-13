import type { MiddlewareConfigFn } from "wasp/server";

/**
 * Strips Wasp's JSON body parser from the Better Auth namespace.
 *
 * Better Auth's `toNodeHandler` reads the raw request stream. If `express.json()`
 * has already consumed it, every Better Auth request hangs with no error -- one
 * of the more confusing failure modes in this integration.
 */
export const rawBodyMiddleware: MiddlewareConfigFn = (middlewareConfig) => {
  middlewareConfig.delete("express.json");
  return middlewareConfig;
};
