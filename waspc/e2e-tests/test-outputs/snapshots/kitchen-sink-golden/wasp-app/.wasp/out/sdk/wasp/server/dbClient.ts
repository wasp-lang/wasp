import { setUpPrisma as setUpPrisma_ext } from 'wasp/src/features/db/prisma'

function createDbClient() {
  return setUpPrisma_ext()
}

const dbClient = createDbClient()

// PUBLIC API 
export type PrismaClient = typeof dbClient

// PUBLIC API
export default dbClient
