{{={= =}=}}
import type { Request, Response, NextFunction } from 'express'
import type { ParamsDictionary, Query } from 'express-serve-static-core'

{=# isAuthEnabled =}
import { type AuthUserData } from './auth/user.js'
{=/ isAuthEnabled =}

type RequestWithExtraFields<
  P = ParamsDictionary,
  ResBody = any,
  ReqBody = any,
  ReqQuery = Query,
  Locals extends Record<string, any> = Record<string, any>,
> = Request<P, ResBody, ReqBody, ReqQuery, Locals> & {
  {=# isAuthEnabled =}
  user: AuthUserData | null;
  sessionId: string | null;
  {=/ isAuthEnabled =}
}

/**
 * Decorator for async express middleware that handles promise rejections.
 * @param {Func} middleware - Express middleware function.
 * @returns Express middleware that is exactly the same as the given middleware but,
 *   if given middleware returns promise, reject of that promise will be correctly handled,
 *   meaning that error will be forwarded to next().
 */
export const handleRejection = <
  P = ParamsDictionary,
  ResBody = any,
  ReqBody = any,
  ReqQuery = Query,
  Locals extends Record<string, any> = Record<string, any>,
>(
  middleware: (
    req: RequestWithExtraFields<P, ResBody, ReqBody, ReqQuery, Locals>,
    res: Response,
    next: NextFunction
  ) => any
) =>
async (req: RequestWithExtraFields<P, ResBody, ReqBody, ReqQuery, Locals>, res: Response, next: NextFunction) => {
  try {
    await middleware(req, res, next)
  } catch (error) {
    next(error)
  }
}

export const sleep = (ms: number): Promise<unknown> => new Promise((r) => setTimeout(r, ms))

export function redirect(res: Response, redirectUri: string)  {
  return res
    .status(302)
    .setHeader("Location", redirectUri)
    .end();
}
