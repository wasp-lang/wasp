
import prisma from '../dbClient.js'

export const contextWithUserEntity = {
  entities: {
    User: prisma.user
  }
}

export const authConfig = {
  failureRedirectPath: "/login",
  successRedirectPath: "/",
}
