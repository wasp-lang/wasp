import { Request, Response, NextFunction } from 'express';
import { type AuthUser } from 'wasp/auth';
type RequestWithExtraFields = Request & {
    user?: AuthUser;
    sessionId?: string;
};
/**
 * Decorator for async express middleware that handles promise rejections.
 * @param {Func} middleware - Express middleware function.
 * @returns Express middleware that is exactly the same as the given middleware but,
 *   if given middleware returns promise, reject of that promise will be correctly handled,
 *   meaning that error will be forwarded to next().
 */
export declare const handleRejection: (middleware: (req: RequestWithExtraFields, res: Response, next: NextFunction) => any) => (req: RequestWithExtraFields, res: Response, next: NextFunction) => Promise<void>;
export declare const sleep: (ms: number) => Promise<unknown>;
export {};
