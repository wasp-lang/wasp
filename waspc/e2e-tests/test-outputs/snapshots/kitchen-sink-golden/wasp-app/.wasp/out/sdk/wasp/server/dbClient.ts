import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { FromRegister } from '../types/register'
import { createPrismaClient as createPrismaClient_ext } from "virtual:wasp/user/features/db/prisma"

// PUBLIC API
export type PrismaClient = ReturnType<RegisteredCreatePrismaClient>;

export type RegisteredCreatePrismaClient = FromRegister<'createPrismaClient', () => InternalPrismaClient>;

const dbClient: PrismaClient =  createPrismaClient_ext();

// PUBLIC API
export default dbClient;
