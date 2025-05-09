import { Response, RequestHandler } from 'express';
import { type AuthUserData } from './auth/user.js';
declare global {
    namespace Express {
        interface Request {
            user?: AuthUserData | null;
            sessionId?: string | null;
        }
    }
}
/**
 * Simple helper to give the correct types for Express handlers.
 * We define it in the same file as our extension to Request
 * so that it is picked up by TypeScript.
 */
export declare const defineHandler: <T extends RequestHandler>(middleware: T) => T;
export declare const sleep: (ms: number) => Promise<unknown>;
export declare function redirect(res: Response, redirectUri: string): Response<any, Record<string, any>>;
//# sourceMappingURL=utils.d.ts.map