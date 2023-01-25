{{={= =}=}}
{=# isPasswordRequired =}
import { v4 as uuidv4 } from 'uuid'
{=/ isPasswordRequired =}

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
  // Attempt to find a User by an external auth association.
  const externalAuthAssociation = await prisma.{= externalAuthEntityLower =}.findFirst({
    where: { provider, providerId },
    include: { user: true }
  })

  if (externalAuthAssociation) {
    return externalAuthAssociation.user
  }

  // No external auth association linkage found. Create a new User using details from
  // `getUserFields()`. Additionally, associate the externalAuthAssociations with the new User.
  // NOTE: For now, we force a random (uuidv4) password string. In the future, we will allow password reset.
  const userFields = await getUserFields()
  const userAndExternalAuthAssociation = {
    ...userFields,
    {=# isPasswordRequired =}
    password: uuidv4(),
    {=/ isPasswordRequired =}
    externalAuthAssociations: {
      create: [{ provider, providerId }]
    }
  }

  return await prisma.{= userEntityLower =}.create({ data: userAndExternalAuthAssociation })
}
