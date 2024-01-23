import type { PrismaClient } from '@prisma/client'

export type DbSeedFn = (prismaClient: PrismaClient) => Promise<void>
