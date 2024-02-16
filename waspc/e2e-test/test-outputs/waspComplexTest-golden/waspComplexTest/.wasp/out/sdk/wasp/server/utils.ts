import crypto from 'crypto'
import { Request, Response, NextFunction } from 'express'

import { readdir } from 'fs'
import { dirname } from 'path'
import { fileURLToPath } from 'url'

import { type AuthUser } from 'wasp/auth'

type RequestWithExtraFields = Request & {
  user?: AuthUser;
  sessionId?: string;
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
