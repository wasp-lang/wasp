import type { PrismaClient } from '@prisma/client'

export type DbSeedFn = (prisma: PrismaClient) => Promise<void>
