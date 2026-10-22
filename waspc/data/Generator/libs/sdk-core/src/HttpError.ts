export interface HttpErrorOptions extends ErrorOptions {
  headers?: Record<string, string>;
}

export class HttpError extends Error {
  public statusCode: number;
  public data: unknown;
  public headers: Record<string, string> | undefined;

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
