import { type RequestHandler } from 'express';
export type MiddlewareConfigFn = (middlewareConfig: MiddlewareConfig) => MiddlewareConfig;
export type MiddlewareConfig = Map<string, RequestHandler>;
//# sourceMappingURL=globalMiddleware.d.ts.map