import { Response, RequestHandler } from 'express'

import { type AuthUserData } from './auth/user.js'

// This is explicitly how Express expects extensions to their
// Request and Response objects to be done.
// https://github.com/DefinitelyTyped/DefinitelyTyped/blob/5d29b9be383902b0399f26072b4590ac61ca72c5/types/express-serve-static-core/index.d.ts#L6-L15
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
export const defineHandler = <T extends RequestHandler>(
  middleware: T
): T => middleware

export const sleep = (ms: number): Promise<unknown> => new Promise((r) => setTimeout(r, ms))

export function redirect(res: Response, redirectUri: string)  {
  return res
    .status(302)
    .setHeader("Location", redirectUri)
    .end();
}
