{{={= =}=}}
import jwt from 'jsonwebtoken'
import SecurePassword from "secure-password"
import Promise from 'bluebird'
import Prisma from '@prisma/client'

const prisma = new Prisma.PrismaClient()

const jwtSign = Promise.promisify(jwt.sign)
const jwtVerify = Promise.promisify(jwt.verify)

// TODO(matija): is this safe, to just define it here for start?
// Let user somehow define this, ideally outside of the codebase, in some .env file.
const JWT_SECRET = "someSecret"

export const sign = (id, options) => jwtSign({ id }, JWT_SECRET, options)
export const verify = (token) => jwtVerify(token, JWT_SECRET)

// TODO(matija): if this function throw an error, who handles it? Seems to be unhandled.
const auth = async (req, res, next) => {
  const authHeader = req.get('Authorization')
  if (!authHeader) {
    // NOTE(matija): for now we let tokenless requests through and make it operation's
    // responsibility to verify whether the request is authenticated or not. In the future
    // we will develop our own system at Wasp-level for that.
    return next()
  }

  if (authHeader.startsWith('Bearer ')) {
    const token = authHeader.substring(7, authHeader.length)
    const {= userEntityLower =}IdFromToken = (await verify(token)).id

    const {= userEntityLower =} = await prisma.{= userEntityLower =}.findOne({ where: { id: {= userEntityLower =}IdFromToken } })
    if (!{= userEntityLower =}) {
      return res.status(401).send()
    }

    const { password, ...{= userEntityLower =}View } = {= userEntityLower =}

    req.{= userEntityLower =} = {= userEntityLower =}View
  } else {
    return res.status(401).send()
  }

  next()
}

export const createNew{= userEntityUpper =} = async ({= userEntityLower =}Fields) => {
  const hashedPassword = await hashPassword({= userEntityLower =}Fields.password)

  const new{= userEntityUpper =} = await prisma.{= userEntityLower =}.create({
    data: {
      ...{= userEntityLower =}Fields,
      password: hashedPassword
    },
  })

  return new{= userEntityUpper =}
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

