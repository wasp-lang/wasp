import type { Request, RequestHandler, Response } from "express";

export type ApiErrorCode =
  | "invalid_filter"
  | "invalid_limit"
  | "invalid_timeout"
  | "invalid_id"
  | "email_not_found"
  | "timeout";

export class ApiError extends Error {
  constructor(
    readonly status: number,
    readonly code: ApiErrorCode,
    message: string,
  ) {
    super(message);
  }
}

/** Responds to an `ApiError` with `{ error: { code, message } }`. */
export function withApiErrorResponses(
  handler: (req: Request, res: Response) => void | Promise<void>,
): RequestHandler {
  return async (req, res, next) => {
    try {
      await handler(req, res);
    } catch (error) {
      if (!(error instanceof ApiError)) {
        return next(error);
      }
      res
        .status(error.status)
        .json({ error: { code: error.code, message: error.message } });
    }
  };
}
