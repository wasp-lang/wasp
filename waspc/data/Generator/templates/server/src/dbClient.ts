{{={= =}=}}
import Prisma from '@prisma/client'

{=# isAuthEnabled =}
import { registerAuthMiddleware } from './core/auth/prismaMiddleware.js'
{=/ isAuthEnabled =}

const createDbClient = () => {
  let prismaClient = new Prisma.PrismaClient()

  {=# isAuthEnabled =}
  prismaClient = registerAuthMiddleware(prismaClient)
  {=/ isAuthEnabled =}

  return prismaClient
}

const dbClient = createDbClient()

export default dbClient
