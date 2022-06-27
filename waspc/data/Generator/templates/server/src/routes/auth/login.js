{{={= =}=}}
import Prisma from '@prisma/client'
import SecurePassword from 'secure-password'

import { verifyPassword } from '../../core/auth.js'
import { handleRejection } from '../../utils.js'

const prisma = new Prisma.PrismaClient()

export default handleRejection(async (req, res) => {
  const args = req.body || {}

  // Try to fetch user with the given email.
  const {= userEntityLower =} = await prisma.{= userEntityLower =}.findUnique({ where: { email: args.email.toLowerCase() } })
  if (!user) {
    return res.status(401).send()
  }

  // We got user - now check the password.
  const verifyPassRes = await verifyPassword({= userEntityLower =}.password, args.password)
  switch (verifyPassRes) {
    case SecurePassword.VALID:
      break
    case SecurePassword.VALID_NEEDS_REHASH:
      // TODO(matija): take neccessary steps to make the password more secure.
      break
    default:
      return res.status(401).send()
  }

  // Save userId in session for future request use.
  // NOTE: Ideally, we should look at what the Primary Key is as defined by Prisma.
  req.session = { userId: {= userEntityLower =}.id }

  return res.status(200).send()
})
