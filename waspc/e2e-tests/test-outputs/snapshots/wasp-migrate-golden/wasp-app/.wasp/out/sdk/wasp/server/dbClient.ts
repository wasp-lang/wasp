import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegister } from '@wasp.sh/lib-sdk-core'

// PUBLIC API
export type PrismaClient = ReturnType<RegisteredPrismaSetupFn>;

export type RegisteredPrismaSetupFn = FromRegister<'prismaSetupFn', () => InternalPrismaClient>;

const dbClient: PrismaClient = new InternalPrismaClient();

// PUBLIC API
export default dbClient;
