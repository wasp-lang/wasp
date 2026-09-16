import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegister } from '../types/register'

function createDbClient() {
  return new InternalPrismaClient({
    // The auth identity's secret material (password hashes, ...) never leaves
    // this column unless auth internals opt back in per query -- so it cannot
    // end up in an operation result or a log by accident.
    omit: {
      authIdentity: {
        providerSecrets: true,
      },
    },
  })
}

// PUBLIC API
export type PrismaClient = ReturnType<RegisteredPrismaSetupFn>;

// NOTE: If the app defines its own `prismaSetupFn`, the client it constructs is
// used as-is -- including any `omit` configuration it chooses (or omits).
export type RegisteredPrismaSetupFn = FromRegister<'prismaSetupFn', typeof createDbClient>;

const dbClient: PrismaClient = createDbClient();

// PUBLIC API
export default dbClient;
