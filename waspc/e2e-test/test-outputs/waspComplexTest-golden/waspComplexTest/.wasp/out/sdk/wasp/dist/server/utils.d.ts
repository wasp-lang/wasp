import { Request, Response, NextFunction } from 'express';
import { type AuthUserData } from './auth/user.js';
type RequestWithExtraFields = Request & {
    user: AuthUserData | null;
    sessionId: string | null;
};
/**
 * Decorator for async express middleware that handles promise rejections.
 * @param {Func} middleware - Express middleware function.
 * @returns Express middleware that is exactly the same as the given middleware but,
 *   if given middleware returns promise, reject of that promise will be correctly handled,
 *   meaning that error will be forwarded to next().
 */
export declare const handleRejection: <Req extends Request = RequestWithExtraFields, Res extends Response = Response>(middleware: (req: Req, res: Res, next: NextFunction) => any) => (req: Req, res: Res, next: NextFunction) => Promise<void>;
export declare const sleep: (ms: number) => Promise<unknown>;
export declare function redirect(res: Response, redirectUri: string): Response<any, Record<string, any>>;
export {};
