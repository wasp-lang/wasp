class AuthError extends Error {
  constructor (message, data, ...params) {
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
