import { createNewUser } from '@wasp/core/auth.js'

export const signup = async ({ username, email, password }, context) => {
  await createNewUser({ username, email, password })
}
