import { Request, Response, NextFunction } from 'express'

import { type AuthUserData } from './auth/user.js'

type RequestWithExtraFields = Request & {
  user: AuthUserData | null;
  sessionId: string | null;
}

/**
 * Decorator for async express middleware that handles promise rejections.
 * @param {Func} middleware - Express middleware function.
 * @returns Express middleware that is exactly the same as the given middleware but,
 *   if given middleware returns promise, reject of that promise will be correctly handled,
 *   meaning that error will be forwarded to next().
 */
export const handleRejection = (
  middleware: (
    req: RequestWithExtraFields,
    res: Response,
    next: NextFunction
  ) => any
) =>
async (req: RequestWithExtraFields, res: Response, next: NextFunction) => {
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
