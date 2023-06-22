{{={= =}=}}
import { verifyPassword, throwInvalidCredentialsError } from '../../../core/auth.js'
import { handleRejection } from '../../../utils.js'

import { findUserBy, createAuthToken } from '../../utils.js'

export default handleRejection(async (req, res) => {
  const args = req.body || {}

  const user = await findUserBy<'username'>({ username: args.username })
  if (!user) {
    throwInvalidCredentialsError()
  }

  try {
    await verifyPassword(user.password, args.password)
  } catch(e) {
    throwInvalidCredentialsError()
  }

  // Username & password valid - generate token.
  const token = await createAuthToken(user)

  // NOTE(matija): Possible option - instead of explicitly returning token here,
  // we could add to response header 'Set-Cookie {token}' directive which would then make
  // browser automatically save cookie with token.
  // NOTE(shayne): Cross-domain cookies have serious limitations, which we recently explored.

  return res.json({ token })
})
