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
declare const auth: (req: any, res: Response, next: NextFunction) => Promise<void>;
export default auth;
