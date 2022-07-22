{{={= =}=}}
import jwt from 'jsonwebtoken'
import SecurePassword from 'secure-password'
import util from 'util'
import { v4 as uuidv4 } from 'uuid'

import prisma from '../dbClient.js'
import { handleRejection } from '../utils.js'
import config from '../config.js'

const jwtSign = util.promisify(jwt.sign)
const jwtVerify = util.promisify(jwt.verify)

const JWT_SECRET = config.auth.jwtSecret

export const sign = (id, options) => jwtSign({ id }, JWT_SECRET, options)
export const verify = (token) => jwtVerify(token, JWT_SECRET)

const auth = handleRejection(async (req, res, next) => {
  const authHeader = req.get('Authorization')
  if (!authHeader) {
    // NOTE(matija): for now we let tokenless requests through and make it operation's
    // responsibility to verify whether the request is authenticated or not. In the future
    // we will develop our own system at Wasp-level for that.
    return next()
  }

  if (authHeader.startsWith('Bearer ')) {
    const token = authHeader.substring(7, authHeader.length)

    let userIdFromToken
    try {
      userIdFromToken = (await verify(token)).id
    } catch (error) {
      if (['TokenExpiredError', 'JsonWebTokenError', 'NotBeforeError'].includes(error.name)) {
        return res.status(401).send()
      } else {
        throw error
      }
    }

    const user = await prisma.{= userEntityLower =}.findUnique({ where: { id: userIdFromToken } })
    if (!user) {
      return res.status(401).send()
    }

    const { password, ...userView } = user

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

// Looks up a user by email and if:
// (1) the user does not exist, create them with a random password, or
// (2) if the user does exist, update their existing password to a random password.
export async function upsertUserWithRandomPassword(email) {
  const randomPassword = uuidv4()

  const user = await prisma.{= userEntityLower =}.upsert({
    where: { email },
    update: { password: randomPassword },
    create: { email, password: randomPassword },
  })

  return user
}

export default auth
