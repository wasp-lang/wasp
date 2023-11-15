{{={= =}=}}
import { verifyPassword, throwInvalidCredentialsError } from '../../../core/auth.js'
import { handleRejection } from '../../../utils.js'

import { findAuthWithUserBy, createAuthToken } from '../../utils.js'
import { ensureValidUsername, ensurePasswordIsPresent } from '../../validation.js'

export default handleRejection(async (req, res) => {
  const userFields = req.body || {}
  ensureValidArgs(userFields)

  const auth = await findAuthWithUserBy({ username: userFields.username })
  if (!auth) {
    throwInvalidCredentialsError()
  }

  try {
    await verifyPassword(auth.password, userFields.password)
  } catch(e) {
    throwInvalidCredentialsError()
  }

  // Username & password valid - generate token.
  const token = await createAuthToken(auth)

  // NOTE(matija): Possible option - instead of explicitly returning token here,
  // we could add to response header 'Set-Cookie {token}' directive which would then make
  // browser automatically save cookie with token.
  // NOTE(shayne): Cross-domain cookies have serious limitations, which we recently explored.

  return res.json({ token })
})

function ensureValidArgs(args: unknown): void {
  ensureValidUsername(args);
  ensurePasswordIsPresent(args);
}
