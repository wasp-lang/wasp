import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegister } from '../types/register'

// PUBLIC API
export type PrismaClient = ReturnType<RegisteredPrismaSetupFn>;

export type RegisteredPrismaSetupFn = FromRegister<'prismaSetupFn', () => InternalPrismaClient>;

import { setUpPrisma as setUpPrisma_ext } from 'virtual:wasp/user/features/db/prisma'
const dbClient: PrismaClient =  setUpPrisma_ext();

// PUBLIC API
export default dbClient;
