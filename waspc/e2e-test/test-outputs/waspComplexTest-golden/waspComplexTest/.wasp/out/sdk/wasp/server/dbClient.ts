import { PrismaClient } from '@prisma/client'

const _waspSetupPrisma = () => new PrismaClient()

// We are not typing the return value because 
// we want to infer the type from the (potentially)
// user-defined function _waspSetupPrisma
function createDbClient() {
  return _waspSetupPrisma()
}

const dbClient = createDbClient()

// PUBLIC API
export default dbClient
