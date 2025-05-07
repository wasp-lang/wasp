import { PrismaClient } from '@prisma/client'

export const setupPrisma = () => {
  const prisma = new PrismaClient({
    // Log SQL queries if needed
    // log: ['query'],
  }).$extends({
    query: {
      task: {
        async findMany({ args, query }) {
          args.where = {
            ...args.where,
            description: { not: { contains: 'hidden by setupPrisma' } },
          }
          return query(args)
        },
      },
    },
  })

  return prisma
}
