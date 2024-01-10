{{={= =}=}}
import { randomInt } from 'node:crypto'

import prisma from '../dbClient.js'
import { handleRejection } from '../utils.js'
import { getSessionAndUserFromBearerToken } from '../auth/session.js'
import { throwInvalidCredentialsError } from '../auth/utils.js'

const auth = handleRejection(async (req, res, next) => {
  const authHeader = req.get('Authorization')
  if (!authHeader) {
    // NOTE(matija): for now we let tokenless requests through and make it operation's
    // responsibility to verify whether the request is authenticated or not. In the future
    // we will develop our own system at Wasp-level for that.
    return next()
  }

  const { session, user } = await getSessionAndUserFromBearerToken(req);

  if (!session || !user) {
    throwInvalidCredentialsError()
  }

  req.sessionId = session.id
  req.user = user

  next()
})

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
  const users = await prisma.{= userEntityLower =}.findMany({
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

export default auth
