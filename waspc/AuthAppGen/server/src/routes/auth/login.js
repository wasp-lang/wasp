import Prisma from '@prisma/client'

import { sign } from '../../auth.js'
import { handleRejection } from '../../utils.js'

const prisma = new Prisma.PrismaClient()

export default handleRejection(async (req, res) => {
  const args = req.body || {}
  const context = {}

  // Try to fetch user with the given email.
  const user = await prisma.user.findOne({ where: { email: args.email.toLowerCase() } })
  if (!user) {
    // TODO: use res
    const error = new Error('No such user.')
    throw error
  }

  // We got user - now check the password.
  // TODO(matija): not even hashed for now, fix that.
  if (args.password !== user.password) {
    // TODO: use res
    throw new Error('Invalid password.')
  }

  // Email & password valid - generate token.
  const token = await sign(user.id)

  // NOTE(matija): Possible option - instead of explicitly returning token here,
  // we could add to response header 'Set-Cookie {token}' directive which would then make
  // browser automatically save cookie with token.

  res.json({ token })
})
