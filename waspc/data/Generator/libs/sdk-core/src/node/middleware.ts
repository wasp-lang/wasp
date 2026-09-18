import type { RequestHandler } from "express";

export type MiddlewareConfigFn = (
  middlewareConfig: MiddlewareConfig,
) => MiddlewareConfig;

// PRIVATE API
export type MiddlewareConfig = Map<string, RequestHandler>;
