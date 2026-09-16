import express from 'express';
import cookieParser from 'cookie-parser';
import logger from 'morgan';
import cors from 'cors';
import helmet from 'helmet';
import { config } from 'wasp/server';
const _waspGlobalMiddlewareConfigFn = (mc) => mc;
// This is the set of middleware Wasp supplies by default.
// NOTE: Remember to update the docs of these change.
const defaultGlobalMiddlewareConfig = new Map([
    ['helmet', helmet()],
    ['cors', cors({ origin: config.allowedCORSOrigins })],
    ['logger', logger('dev')],
    ['express.json', express.json()],
    ['express.urlencoded', express.urlencoded()],
    ['cookieParser', cookieParser()]
]);
// This is the global middleware that is the result of applying the user's modifications.
// It will be used as the basis for Operations and APIs (unless they are further customized).
const globalMiddlewareConfig = _waspGlobalMiddlewareConfigFn(defaultGlobalMiddlewareConfig);
// This function returns an array of Express middleware to be used by a router. It optionally
// accepts a function that can modify the global middleware for specific route customization.
export function globalMiddlewareConfigForExpress(middlewareConfigFn) {
    if (!middlewareConfigFn) {
        return Array.from(globalMiddlewareConfig.values());
    }
    // Make a clone so they can't mess up the global Map for any other routes calling this.
    const globalMiddlewareConfigClone = new Map(globalMiddlewareConfig);
    const modifiedMiddlewareConfig = middlewareConfigFn(globalMiddlewareConfigClone);
    return Array.from(modifiedMiddlewareConfig.values());
}
//# sourceMappingURL=globalMiddleware.js.map