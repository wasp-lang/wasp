import { type RequestHandler } from 'express'

// PUBLIC API
export type MiddlewareConfigFn = (middlewareConfig: MiddlewareConfig) => MiddlewareConfig

// PRIVATE API
export type MiddlewareConfig = Map<string, RequestHandler>

// Error logging middleware for jobs
export const errorLoggingMiddleware = (jobHandler: Function) => {
  return async (...args: any[]) => {
    try {
      await jobHandler(...args);
    } catch (error) {
      console.error("Job error:", error);
      // Prevent job from crashing the server
    }
  };
};
