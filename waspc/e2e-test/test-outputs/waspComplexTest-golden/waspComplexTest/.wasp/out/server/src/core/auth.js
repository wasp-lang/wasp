import jwt from 'jsonwebtoken'
import SecurePassword from 'secure-password'
import util from 'util'
import { randomInt } from 'node:crypto'

import prisma from '../dbClient.js'
import { handleRejection } from '../utils.js'
import HttpError from '../core/HttpError.js'
import config from '../config.js'
import { deserializeAndSanitizeProviderData } from '../auth/utils.js'

const jwtSign = util.promisify(jwt.sign)
const jwtVerify = util.promisify(jwt.verify)

const JWT_SECRET = config.auth.jwtSecret

export const signData = (data, options) => jwtSign(data, JWT_SECRET, options)
export const sign = (id, options) => signData({ id }, options)
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
    req.user = await getUserFromToken(token)
  } else {
    throwInvalidCredentialsError()
  }

  next()
})

export async function getUserFromToken(token) {
  let userIdFromToken
  try {
    userIdFromToken = (await verify(token)).id
  } catch (error) {
    if (['TokenExpiredError', 'JsonWebTokenError', 'NotBeforeError'].includes(error.name)) {
      throwInvalidCredentialsError()
    } else {
      throw error
    }
  }

  const user = await prisma.user
    .findUnique({
      where: { id: userIdFromToken },
      include: {
        auth: {
          include: {
            identities: true
          }
        }
      }
    })
  if (!user) {
    throwInvalidCredentialsError()
  }

  // TODO: This logic must match the type in types/index.ts (if we remove the
  // password field from the object here, we must to do the same there).
  // Ideally, these two things would live in the same place:
  // https://github.com/wasp-lang/wasp/issues/965
  let sanitizedUser = { ...user }
  sanitizedUser.auth.identities = sanitizedUser.auth.identities.map(identity => {
    identity.providerData = deserializeAndSanitizeProviderData(identity.providerData, { shouldRemovePasswordField: true })
    return identity
  });
  return sanitizedUser
}

const SP = new SecurePassword()

export const hashPassword = async (password) => {
  const hashedPwdBuffer = await SP.hash(Buffer.from(password))
  return hashedPwdBuffer.toString("base64")
}

export const verifyPassword = async (hashedPassword, password) => {
  const result = await SP.verify(Buffer.from(password), Buffer.from(hashedPassword, "base64"))
  if (result !== SecurePassword.VALID) {
    throw new Error('Invalid password.')
  }
}

// Generates an unused username that looks similar to "quick-purple-sheep-91231". 
// It generates several options and ensures it picks one that is not currently in use.
export function generateAvailableDictionaryUsername() {
  const adjectives = ['fuzzy', 'tall', 'short', 'nice', 'happy', 'quick', 'slow', 'good', 'new', 'old', 'first', 'last', 'old', 'young']
  const colors = ['red', 'green', 'blue', 'white', 'black', 'brown', 'purple', 'orange', 'yellow']
  const nouns = ['wasp', 'cat', 'dog', 'lion', 'rabbit', 'duck', 'pig', 'bee', 'goat', 'crab', 'fish', 'chicken', 'horse', 'llama', 'camel', 'sheep']

  const potentialUsernames = []
  for (let i = 0; i < 10; i++) {
    const potentialUsername = `${adjectives[randomInt(adjectives.length)]}-${colors[randomInt(colors.length)]}-${nouns[randomInt(nouns.length)]}-${randomInt(100_000)}`
    potentialUsernames.push(potentialUsername)
  }

  return findAvailableUsername(potentialUsernames)
}

// Generates an unused username based on an array of username segments and a separator. 
// It generates several options and ensures it picks one that is not currently in use.
export function generateAvailableUsername(usernameSegments, config) {
  const separator = config?.separator || '-'
  const baseUsername = usernameSegments.join(separator)

  const potentialUsernames = []
  for (let i = 0; i < 10; i++) {
    const potentialUsername = `${baseUsername}${separator}${randomInt(100_000)}`
    potentialUsernames.push(potentialUsername)
  }

  return findAvailableUsername(potentialUsernames)
}

// Checks the database for an unused username from an array provided and returns first.
async function findAvailableUsername(potentialUsernames) {
  const users = await prisma.user.findMany({
    where: {
      username: { in: potentialUsernames },
    }
  })
  const takenUsernames = users.map(user => user.username)
  const availableUsernames = potentialUsernames.filter(username => !takenUsernames.includes(username))

  if (availableUsernames.length === 0) {
    throw new Error('Unable to generate a unique username. Please contact Wasp.')
  }

  return availableUsernames[0]
}

export function throwInvalidCredentialsError(message) {
  throw new HttpError(401, 'Invalid credentials', { message })
}

export default auth
