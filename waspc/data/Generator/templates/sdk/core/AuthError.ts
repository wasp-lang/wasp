class AuthError extends Error {
  public data: unknown

  constructor (message: string, data?: unknown, ...params: unknown[]) {
    super(message, ...params)

    if (Error.captureStackTrace) {
      Error.captureStackTrace(this, AuthError)
    }

    this.name = this.constructor.name

    if (data) {
      this.data = data
    }
  }
}

export default AuthError
