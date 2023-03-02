class HttpError extends Error {
  constructor (statusCode, message, data, ...params) {
    super(message, ...params)

    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, HttpError)
    }

    this.name = this.constructor.name

    if (!(Number.isInteger(statusCode) && statusCode >= 400 && statusCode < 600)) {
      throw new Error('statusCode has to be integer in range [400, 600).')
    }
    this.statusCode = statusCode

    if (data) {
      this.data = data
    }
  }
}

export default HttpError
