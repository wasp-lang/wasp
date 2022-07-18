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

export async function findOrCreateUserEntity(email) {
  const password = uuidv4()

  // NOTE(shayne): We cannot easily use the "upsert" trick to "find or create" users that Prisma recommends
  // since we apply checks to email and password as part of this (so we cannot `update: {}`).
  // Therefore, while this could theoretically have a race condition with multiple node servers operating
  // on the same new user at the same time, the practical odds are so low it doesn't seem like getting the
  // other approach working is justified right now.
  // Ref: https://github.com/prisma/docs/issues/640
  let user = await prisma.{= userEntityLower =}.findUnique({ where: { email } })
  if (!user) {
    user = await prisma.{= userEntityLower =}.create({ data: { email, password } })
  }
  return user
}

export default auth
