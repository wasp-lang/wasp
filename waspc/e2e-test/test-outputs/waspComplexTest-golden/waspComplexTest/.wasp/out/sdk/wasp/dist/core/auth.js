import { handleRejection } from 'wasp/server/utils';
import { getSessionAndUserFromBearerToken } from 'wasp/auth/session';
import { throwInvalidCredentialsError } from 'wasp/auth/utils';
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
const auth = handleRejection(async (req, res, next) => {
    const authHeader = req.get('Authorization');
    if (!authHeader) {
        // NOTE(matija): for now we let tokenless requests through and make it operation's
        // responsibility to verify whether the request is authenticated or not. In the future
        // we will develop our own system at Wasp-level for that.
        return next();
    }
    const { session, user } = await getSessionAndUserFromBearerToken(req);
    if (!session || !user) {
        throwInvalidCredentialsError();
    }
    req.sessionId = session.id;
    req.user = user;
    next();
});
export default auth;
//# sourceMappingURL=auth.js.map