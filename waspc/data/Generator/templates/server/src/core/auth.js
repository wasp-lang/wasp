{{={= =}=}}
import jwt from 'jsonwebtoken'
import SecurePassword from 'secure-password'
import util from 'util'

import prisma from '../dbClient.js'
import { handleRejection } from '../utils.js'

const jwtSign = util.promisify(jwt.sign)
const jwtVerify = util.promisify(jwt.verify)

// TODO(matija): this is not safe, this value should come from some config file/environment
// and shouldn't be commited to the version control.
const JWT_SECRET = "developmentJwtSecret"

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
    const userIdFromToken = (await verify(token)).id

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

// TODO(matija): since this function is not doing much anymore, we can remove it.
// Make sure to replace its invocations with direct calls to prisma client's create().
// Github issue: https://github.com/wasp-lang/wasp/issues/150
export const createNewUser = async (userFields) => {
  const newUser = await prisma.{= userEntityLower =}.create({ data: userFields })

  return newUser
}

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

