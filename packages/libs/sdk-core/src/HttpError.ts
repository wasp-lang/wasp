/**
 * The header values Node's `response.setHeader` accepts, e.g. a list of
 * `Set-Cookie` values.
 */
export type HttpErrorHeaders = Record<string, string | number | string[]>;

export interface HttpErrorOptions extends ErrorOptions {
  headers?: HttpErrorHeaders;
}

export class HttpError extends Error {
  public statusCode: number;
  public data: unknown;
  public headers: HttpErrorHeaders | undefined;

  constructor(
    statusCode: number,
    message?: string,
    data?: Record<string, unknown>,
    options?: HttpErrorOptions,
  ) {
    super(message, options);

    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, HttpError);
    }

    this.name = this.constructor.name;

    if (
      !(Number.isInteger(statusCode) && statusCode >= 400 && statusCode < 600)
    ) {
      throw new Error("statusCode has to be integer in range [400, 600).");
    }
    this.statusCode = statusCode;

    if (data) {
      this.data = data;
    }

    this.headers = options?.headers;
  }
}
