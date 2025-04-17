import { getSessionAndUserFromBearerToken } from 'wasp/auth/session';
import { createInvalidCredentialsError } from 'wasp/auth/utils';
import { defineHandler } from 'wasp/server/utils';
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
const auth = defineHandler(async (req, res, next) => {
    const authHeader = req.get('Authorization');
    // NOTE(matija): for now we let tokenless requests through and make it operation's
    // responsibility to verify whether the request is authenticated or not. In the future
    // we will develop our own system at Wasp-level for that.
    if (!authHeader) {
        req.sessionId = null;
        req.user = null;
        return next();
    }
    const sessionAndUser = await getSessionAndUserFromBearerToken(req);
    if (sessionAndUser === null) {
        throw createInvalidCredentialsError();
    }
    req.sessionId = sessionAndUser.session.id;
    req.user = sessionAndUser.user;
    next();
});
export default auth;
//# sourceMappingURL=auth.js.map