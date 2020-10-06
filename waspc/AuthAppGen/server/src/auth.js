import jwt from 'jsonwebtoken'
import Promise from 'bluebird'
import Prisma from '@prisma/client'

const prisma = new Prisma.PrismaClient()

const jwtSign = Promise.promisify(jwt.sign)
const jwtVerify = Promise.promisify(jwt.verify)

// TODO(matija): is this safe, to just define it here for the start?
const JWT_SECRET = "someSecret"

export const sign = (id, options) => jwtSign({ id }, JWT_SECRET, options)
export const verify = (token) => jwtVerify(token, JWT_SECRET)

// TODO(matija): if this function throw an error, who handles it? Seems to be unhandled.
const auth = async (req, res, next) => {
  const authHeader = req.get('Authorization')
  if (!authHeader) {
    return res.status(401).json({ message: 'auth token missing.' })
  }

  if (authHeader.startsWith('Bearer ')) {
    const token = authHeader.substring(7, authHeader.length)
    const userIdFromToken = (await verify(token)).id

    const user = await prisma.user.findOne({ where: { id: userIdFromToken } })
    // Currently we require user to be authenticated. We could also just try to get
    // user if there is one, and then let the operation decide itself if it needs to be
    // private or not.
    if (!user) {
      return res.status(401).send()
    }

    // We should also let Wasp dev decide what fields they want to forward to the operations.
    const { password, ...userView } = user

    req.user = userView
  } else {
    return res.status(401).send()
  }

  next()
}

export const createNewUser = async (userFields) => {
  const newUser = await prisma.user.create({
    data: {
      ...userFields,
      // TODO(matija): we would take care of hashing password.
      //password: hashPassword(userFields.password)
    },
  })

  return newUser
}

export default auth
