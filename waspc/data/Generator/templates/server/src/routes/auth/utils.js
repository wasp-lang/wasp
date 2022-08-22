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

// TODO: Use template helpers instead of prisma.tableNames
export async function findOrCreateUserBySocialLogin(provider, providerId, firstSignInConfigPromise) {
  // Attempt to find User by an associated SocialLogin.
  const socialLogin = await prisma.socialLogin.findFirst({
    where: { provider, providerId },
    include: { user: true },
  })

  if (socialLogin) {
    return { user: socialLogin.user, created: false }
  }

  // No SocialLogin linkage found. Create a new User using details from
  // firstSignInConfigPromise. Additionally, associate the SocialLogin with the new User.
  const userFields = (await firstSignInConfigPromise).userFields
  const userAndSocialLogin = {
    ...userFields,
    password: uuidv4(),
    socialLogins: {
      create: [{ provider, providerId }]
    }
  }

  return {
    user: await prisma.user.create({ data: { ...userAndSocialLogin } }),
    created: true
  }
}

export async function generateAvailableUsername() {
  const adjectives = ['fuzzy', 'tall', 'short', 'nice', 'happy', 'quick', 'slow', 'good', 'new', 'old', 'first', 'last', 'old', 'young']
  const colors = ['red', 'green', 'blue', 'white', 'black', 'brown', 'purple', 'orange', 'yellow']
  const nouns = ['cat', 'dog', 'lion', 'rabbit', 'duck', 'pig', 'bee', 'goat', 'crab', 'fish', 'chicken', 'horse', 'llama', 'camel', 'sheep']

  const potentialUsernames = []
  for (let i = 0; i < 10; i++) {
    const potentialUsername = `${adjectives[randomInt(adjectives.length)]}-${colors[randomInt(colors.length)]}-${nouns[randomInt(nouns.length)]}-${randomInt(100_000)}`
    potentialUsernames.push(potentialUsername)
  }

  // TODO: Change `email` to `username` after merge.
  const users = await prisma.user.findMany({
    where: {
      email: { in: potentialUsernames },
    }
  })
  const takenUsernames = users.map(user => user.email)
  const availableUsernames = potentialUsernames.filter(username => !takenUsernames.includes(username))

  console.log(potentialUsernames, takenUsernames, availableUsernames)

  return availableUsernames[0]
}
