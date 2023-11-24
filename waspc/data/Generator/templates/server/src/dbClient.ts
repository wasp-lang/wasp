{{={= =}=}}
import Prisma from '@prisma/client'

{=# isAuthEnabled =}
import { registerAuthMiddleware } from './core/auth/prismaMiddleware.js'
{=/ isAuthEnabled =}

const createDbClient = () => {
  const prismaClient = new Prisma.PrismaClient({
    log: ['query', 'info', 'warn', 'error'],
  })

  {=# isAuthEnabled =}
  registerAuthMiddleware(prismaClient)
  {=/ isAuthEnabled =}

  return prismaClient
}

const dbClient = createDbClient()

export default dbClient
