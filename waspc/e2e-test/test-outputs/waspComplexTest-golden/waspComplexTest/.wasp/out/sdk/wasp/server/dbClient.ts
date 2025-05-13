import { PrismaClient as InternalPrismaClient } from '@prisma/client'

const _waspSetUpPrisma = () => new InternalPrismaClient()

// We are not typing the return value because 
// we want to infer the type from the (potentially)
// user-defined function _waspSetUpPrisma
function createDbClient() {
  return _waspSetUpPrisma()
}

const dbClient = createDbClient()

// PUBLIC API 
export type PrismaClient = typeof dbClient

// PUBLIC API
export default dbClient
