import Prisma from '@prisma/client'


const createDbClient = () => {
  const prisma = new Prisma.PrismaClient()

  return prisma
}

const dbClient = createDbClient()

// PUBLIC API
export default dbClient
