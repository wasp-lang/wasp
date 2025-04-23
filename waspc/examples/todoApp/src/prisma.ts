import { PrismaSetupFn } from 'wasp/server'
import { PrismaClient } from '@prisma/client'

// Logs SQL queries and their execution time
export const setupPrisma = (() => {
  const prisma = new PrismaClient({
    log: [
      {
        emit: 'event',
        level: 'query',
      },
      {
        emit: 'event',
        level: 'info',
      },
      {
        emit: 'event',
        level: 'warn',
      },
      {
        emit: 'event',
        level: 'error',
      },
    ],
  })

  prisma.$on('query', (e) => {
    console.log(
      `Query: ${e.query} \nParams: ${e.params} \nDuration: ${e.duration}ms`
    )
  })

  return prisma
}) satisfies PrismaSetupFn
