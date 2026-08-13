import { toNodeHandler } from "better-auth/node";
import type { Request, Response } from "express";

import { auth } from "./betterAuth";

const handler = toNodeHandler(auth);

/**
 * Mounts Better Auth's own HTTP surface (sign-up, sign-in, sign-out, OAuth
 * callbacks) inside the Wasp server.
 *
 * Worth noticing: the provider interface has no "mount routes" capability, and
 * does not need one. Wasp already has a general mechanism for user-defined
 * endpoints, so a provider that owns routes just uses it. That keeps the auth
 * interface small.
 *
 * `toNodeHandler` needs the raw request stream, so this route opts out of
 * Wasp's JSON body parser -- see `apiNamespace` in main.wasp.ts.
 */
export const betterAuthRoutes = (
  req: Request,
  res: Response,
  _context: unknown,
) => handler(req, res);
