import Prisma from '@prisma/client'


const createDbClient = () => {
  const prismaClient = new Prisma.PrismaClient()


  return prismaClient
}

const dbClient = createDbClient()

export default dbClient
