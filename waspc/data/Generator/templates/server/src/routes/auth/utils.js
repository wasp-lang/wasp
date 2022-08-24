{{={= =}=}}

import { v4 as uuidv4 } from 'uuid'

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

export async function findOrCreateUserBySocialLogin(provider, providerId, getUserFieldsAsync) {
  // Attempt to find a User by an associated SocialLogin.
  const socialLogin = await prisma.{= socialLoginEntityLower =}.findFirst({
    where: { provider, providerId },
    include: { user: true }
  })

  if (socialLogin) {
    return socialLogin.user
  }

  // No SocialLogin linkage found. Create a new User using details from
  // `getUserFieldsAsync()`. Additionally, associate the SocialLogin with the new User.
  const userFields = await getUserFieldsAsync()
  const userAndSocialLogin = {
    ...userFields,
    password: uuidv4(),
    socialLogins: {
      create: [{ provider, providerId }]
    }
  }

  return await prisma.{= userEntityLower =}.create({ data: userAndSocialLogin })
}
