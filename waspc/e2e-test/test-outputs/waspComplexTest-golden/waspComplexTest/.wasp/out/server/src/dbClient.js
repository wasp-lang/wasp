import Prisma from '@prisma/client'


import { registerAuthMiddleware } from './core/auth/prismaMiddleware.js'


const createDbClient = () => {
  const prismaClient = new Prisma.PrismaClient()


  registerAuthMiddleware(prismaClient)


  return prismaClient
}

const dbClient = createDbClient()

export default dbClient
