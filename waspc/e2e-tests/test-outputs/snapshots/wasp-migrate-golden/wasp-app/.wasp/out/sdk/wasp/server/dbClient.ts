import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegister } from '../types/register'

function createDbClient() {
  return new InternalPrismaClient()
}

// PUBLIC API
export type PrismaClient = ReturnType<RegisteredPrismaSetupFn>;

// NOTE: If the app defines its own `prismaSetupFn`, the client it constructs is
// used as-is -- including any `omit` configuration it chooses (or omits).
export type RegisteredPrismaSetupFn = FromRegister<'prismaSetupFn', typeof createDbClient>;

const dbClient: PrismaClient = createDbClient();

// PUBLIC API
export default dbClient;
