import Prisma from '@prisma/client'
import jwt from 'jsonwebtoken'
import Promise from 'bluebird'

import { handleRejection } from '../../utils.js'

const jwtSign = Promise.promisify(jwt.sign)
const jwtVerify = Promise.promisify(jwt.verify)

const prisma = new Prisma.PrismaClient()

export default handleRejection(async (req, res) => {
  const args = req.body || {}
  const context = {}

  // Try to fetch user with the given email.
  const user = await prisma.user.findOne({ where: { email: args.email.toLowerCase() } })
  if (!user) {
    const error = new Error('No such user.')
    throw error
  }

  // We got user - now check the password.
  // TODO(matija): not even hashed for now, fix that.
  if (args.password !== user.password) {
    throw new Error('Invalid password.')
  }

  // Email & password valid - generate token.
  const token = await jwtSign({ is: user.id }, "someSecret")

  // NOTE(matija): Possible option - instead of explicitly returning token here,
  // we could add to response header 'Set-Cookie {token}' directive which would then make
  // browser automatically save cookie with token.

  res.json({ token })
})
