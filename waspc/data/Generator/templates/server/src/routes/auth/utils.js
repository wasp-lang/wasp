{{={= =}=}}

import { v4 as uuidv4 } from 'uuid'
import { randomInt } from 'node:crypto'

import prisma from '../../dbClient.js'

export const contextWithUserEntity = {
  entities: {
    {= userEntityUpper =}: prisma.{= userEntityLower =}
  }
}

export const authConfig = {
  failureRedirectPath: "{= failureRedirectPath =}",
  successRedirectPath: "{= successRedirectPath =}",
}

export async function findOrCreateUserBySocialLogin(provider, providerId, userFieldsPromise) {
  // Attempt to find a User by an associated SocialLogin.
  const socialLogin = await prisma.{= socialLoginEntityLower =}.findFirst({
    where: { provider, providerId },
    include: { user: true },
  })

  if (socialLogin) {
    return socialLogin.user
  }

  // No SocialLogin linkage found. Create a new User using details from
  // `userFieldsPromise`. Additionally, associate the SocialLogin with the new User.
  const userFields = await userFieldsPromise
  const userAndSocialLogin = {
    ...userFields,
    password: uuidv4(),
    socialLogins: {
      create: [{ provider, providerId }]
    }
  }

  return await prisma.{= userEntityLower =}.create({ data: userAndSocialLogin })
}

export async function generateAvailableDictionaryUsername() {
  const adjectives = ['fuzzy', 'tall', 'short', 'nice', 'happy', 'quick', 'slow', 'good', 'new', 'old', 'first', 'last', 'old', 'young']
  const colors = ['red', 'green', 'blue', 'white', 'black', 'brown', 'purple', 'orange', 'yellow']
  const nouns = ['cat', 'dog', 'lion', 'rabbit', 'duck', 'pig', 'bee', 'goat', 'crab', 'fish', 'chicken', 'horse', 'llama', 'camel', 'sheep']

  const potentialUsernames = []
  for (let i = 0; i < 10; i++) {
    const potentialUsername = `${adjectives[randomInt(adjectives.length)]}-${colors[randomInt(colors.length)]}-${nouns[randomInt(nouns.length)]}-${randomInt(100_000)}`
    potentialUsernames.push(potentialUsername)
  }

  return findAvailableUsername(potentialUsernames)
}

export async function generateAvailableUsername(strings, config) {
  const separator = config?.separator || '-'
  const baseUsername = strings.join(separator)

  const potentialUsernames = []
  for (let i = 0; i < 10; i++) {
    const potentialUsername = `${baseUsername}-${randomInt(100_000)}`
    potentialUsernames.push(potentialUsername)
  }

  return findAvailableUsername(potentialUsernames)
}

async function findAvailableUsername(potentialUsernames) {
  // TODO: Change `email` to `username` after merge.
  const users = await prisma.{= userEntityLower =}.findMany({
    where: {
      email: { in: potentialUsernames },
    }
  })
  const takenUsernames = users.map(user => user.email)
  const availableUsernames = potentialUsernames.filter(username => !takenUsernames.includes(username))

  if (availableUsernames.length === 0) {
    throw 'No available dictionary names. Please contact Wasp.'
  }

  return availableUsernames[0]
}
