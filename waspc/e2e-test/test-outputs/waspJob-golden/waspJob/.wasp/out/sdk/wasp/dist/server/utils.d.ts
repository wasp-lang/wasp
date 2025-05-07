import { Response, RequestHandler } from 'express';
declare global {
    namespace Express {
        interface Request {
        }
    }
}
/**
 * Simple helper to give the correct types for Express handlers.
 * We define it in the same file as our extension to Request
 * so that it is picked up by TypeScript.
 */
export declare const defineHandler: (middleware: RequestHandler) => RequestHandler<import("express-serve-static-core").ParamsDictionary, any, any, import("qs").ParsedQs, Record<string, any>>;
export declare const sleep: (ms: number) => Promise<unknown>;
export declare function redirect(res: Response, redirectUri: string): Response<any, Record<string, any>>;
//# sourceMappingURL=utils.d.ts.map