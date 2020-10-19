{{={= =}=}}
import jwt from 'jsonwebtoken'
import SecurePassword from 'secure-password'
import util from 'util'
import Prisma from '@prisma/client'

import { handleRejection } from '../utils.js'

const prisma = new Prisma.PrismaClient()

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

    const user = await prisma.{= userEntityLower =}.findOne({ where: { id: userIdFromToken } })
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

export const createNewUser = async (userFields) => {
  const hashedPassword = await hashPassword(userFields.password)

  const newUser = await prisma.{= userEntityLower =}.create({
    data: {
      ...userFields,
      password: hashedPassword
    },
  })

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

