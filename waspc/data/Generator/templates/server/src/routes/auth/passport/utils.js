{{={= =}=}}

import prisma from '../../../dbClient.js'

export const contextWithUserEntity = { entities: { {= userEntityUpper =}: prisma.{= userEntityLower =} } }

export const authConfig =  {
  failureRedirectPath: "{= failureRedirectPath =}",
  successRedirectPath: "{= successRedirectPath =}",
}
