{{={= =}=}}
import SecurePassword from 'secure-password'

import prisma from '../dbClient.js'
import { handleRejection } from '../utils.js'

const auth = handleRejection(async (req, res, next) => {
  const session = req.session
  if (!session) {
    // NOTE(matija): for now we let sessionless requests through and make it operation's
    // responsibility to verify whether the request is authenticated or not. In the future
    // we will develop our own system at Wasp-level for that.
    return next()
  }

  const userIdFromSession = session.user_id

  if (userIdFromSession) {
    const user = await prisma.{= userEntityLower =}.findUnique({ where: { id: userIdFromSession } })
    if (!user) {
      return res.status(401).send()
    }

    const { password, ...userView } = user

    console.log("Setting userView: ", userView)
    req.user = userView
  } else {
    return res.status(401).send()
  }

  next()
})

const SP = new SecurePassword()

export const hashPassword = async (password) => {
  const hashedPwdBuffer = await SP.hash(Buffer.from(password))
  return hashedPwdBuffer.toString("base64")
}

export const verifyPassword = async (hashedPassword, password) => {
  try {
    return await SP.verify(Buffer.from(password), Buffer.from(hashedPassword, "base64"))
  } catch (error) {
    console.error(error)
    return false
  }
}

export default auth
