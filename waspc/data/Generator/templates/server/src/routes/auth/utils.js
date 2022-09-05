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

export async function findOrCreateUserByExternalAuthAssociation(provider, providerId, getUserFields) {
  // Attempt to find a User by an associated ExternalAuthAssociation.
  const externalAuthAssociation = await prisma.{= externalAuthAssociationEntityLower =}.findFirst({
    where: { provider, providerId },
    include: { user: true }
  })

  if (externalAuthAssociation) {
    return externalAuthAssociation.user
  }

  // No ExternalAuthAssociation linkage found. Create a new User using details from
  // `getUserFields()`. Additionally, associate the ExternalAuthAssociation with the new User.
  // NOTE: For now, we force a random (uuidv4) password string. In the future, we will allow password reset.
  const userFields = await getUserFields()
  const userAndExternalAuthAssociation = {
    ...userFields,
    password: uuidv4(),
    externalAuthAssociations: {
      create: [{ provider, providerId }]
    }
  }

  return await prisma.{= userEntityLower =}.create({ data: userAndExternalAuthAssociation })
}
