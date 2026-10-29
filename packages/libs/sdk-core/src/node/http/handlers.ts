import type { RequestHandler, Response } from "express";
export const defineHandler = <T extends RequestHandler>(middleware: T): T =>
  middleware;

export function redirect(res: Response, redirectUri: string) {
  return res.status(302).setHeader("Location", redirectUri).end();
}
