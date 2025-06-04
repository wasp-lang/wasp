import { PrismaClient as InternalPrismaClient } from '@prisma/client'

function createDbClient() {
  return new InternalPrismaClient()
}

const dbClient = createDbClient()

// PUBLIC API 
export type PrismaClient = typeof dbClient

// PUBLIC API
export default dbClient
