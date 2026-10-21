import { defineHandler } from "@wasp.sh/lib-sdk-core/node";
import type { RequestHandler } from "express";
import { createInvalidCredentialsError } from "../errors.js";
import type { Sessions } from "./sessions.js";

/**
 * The fields the auth middleware sets on the Express request. The generated
 * server declares them on `Express.Request` with the app's `AuthUserData`.
 */
export type RequestWithAuth<AuthUserData> = {
  user?: AuthUserData | null;
  sessionId?: string | null;
};

/**
 * Creates the auth middleware.
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
export function createAuthMiddleware<AuthUserData>(
  sessions: Sessions<AuthUserData>,
): RequestHandler {
  return defineHandler(async (req, _res, next) => {
    const request = req as typeof req & RequestWithAuth<AuthUserData>;
    const authHeader = req.get("Authorization");
    // NOTE(matija): for now we let tokenless requests through and make it operation's
    // responsibility to verify whether the request is authenticated or not. In the future
    // we will develop our own system at Wasp-level for that.
    if (!authHeader) {
      request.sessionId = null;
      request.user = null;
      return next();
    }

    const sessionAndUser = await sessions.getSessionAndUserFromBearerToken(req);

    if (sessionAndUser === null) {
      throw createInvalidCredentialsError();
    }

    request.sessionId = sessionAndUser.session.id;
    request.user = sessionAndUser.user;

    next();
  });
}
