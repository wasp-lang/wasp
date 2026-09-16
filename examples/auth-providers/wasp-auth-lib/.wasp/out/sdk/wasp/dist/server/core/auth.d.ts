/**
 * Auth middleware
 *
 * If the request includes an `Authorization` header it will try to authenticate the request,
 * otherwise it will let the request through.
 *
 * - If authentication succeeds it sets `req.sessionId` and `req.user`
 *   - `req.user` is the user that made the request and it's used in
 *      all Wasp features that need to know the user that made the request.
 *   - `req.sessionId` is the ID of the session that authenticated the request.
 * - If the request is not authenticated, it throws an error.
 */
declare const auth: (req: import("express-serve-static-core").Request<import("express-serve-static-core").ParamsDictionary, any, any, import("qs").ParsedQs, Record<string, any>>, res: import("express-serve-static-core").Response<any, Record<string, any>, number>, next: import("express-serve-static-core").NextFunction) => Promise<void>;
export default auth;
/**
 * Middleware factory for provider-restricted operations and APIs
 * (`auth: ["wasp", ...]`). Unlike plain `auth: true` (which attaches the user
 * and leaves the check to the operation), the restricted form is
 * self-enforcing: naming providers means "require a session from one of
 * these", so Wasp gates it. No session is a 401 (go log in); a valid session
 * from a non-listed provider is a 403 (logged in, but not like this) -- the
 * distinction that keeps clients from redirecting an already-logged-in user
 * back to the login page. A pure comparison against the provider recorded on
 * the session at mint time; no provider code runs.
 */
export declare function requireSessionProvider(requiredProviderIds: string[]): (req: import("express-serve-static-core").Request<import("express-serve-static-core").ParamsDictionary, any, any, import("qs").ParsedQs, Record<string, any>>, _res: import("express-serve-static-core").Response<any, Record<string, any>, number>, next: import("express-serve-static-core").NextFunction) => Promise<void>;
//# sourceMappingURL=auth.d.ts.map