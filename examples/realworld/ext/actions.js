import { createNewUser } from '@wasp/core/auth.js'
import HttpError from '@wasp/core/HttpError.js'

export const signup = async ({ username, email, password }, context) => {
  try {
    await createNewUser({ username, email, password })
  } catch (err) {
    // TODO: I wish I didn't have to do this, I would love this to be in some
    //   degree done automatically.
    if (err.code == 'P2002') {
      throw new HttpError(400, err.meta.target + " must be unique.")
    }
    throw err
  }
}
