{{={= =}=}}
import Prisma from '@prisma/client'
import SecurePassword from 'secure-password'

import { verifyPassword } from '../../core/auth.js'
import { handleRejection } from '../../utils.js'

const prisma = new Prisma.PrismaClient()

export default handleRejection(async (req, res, next) => {
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

  // Regenerate the session, which is good practice to help
  // guard against forms of session fixation. Ref: Express docs.
  req.session.regenerate(function (err) {
    if (err) next(err)

    // Save user_id in session for future request use.
    req.session.user_id = {= userEntityLower =}.id

    // Save the session before redirection to ensure page
    // load does not happen before session is saved.
    req.session.save(function (err) {
      if (err) return next(err)
      return res.status(200).send()
    })
  })
})
