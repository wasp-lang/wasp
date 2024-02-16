import Prisma from '@prisma/client'

function createDbClient(): Prisma.PrismaClient {
  return new Prisma.PrismaClient()
}

const dbClient = createDbClient()

// PUBLIC API
export default dbClient
