export class HttpError extends Error {
  public statusCode: number
  public data: unknown

  constructor(
    statusCode: number,
    message?: string,
    data?: Record<string, unknown>,
    options?: ErrorOptions
  ) {
    super(message, options)

    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, HttpError)
    }

    this.name = this.constructor.name

    if (
      !(Number.isInteger(statusCode) && statusCode >= 400 && statusCode < 600)
    ) {
      throw new Error('statusCode has to be integer in range [400, 600).')
    }
    this.statusCode = statusCode

    if (data) {
      this.data = data
    }
  }
}
