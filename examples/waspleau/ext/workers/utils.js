import Prisma from '@prisma/client'

const prisma = new Prisma.PrismaClient()

export function upsertMetric({ name, value } = {}) {
  return prisma.metric.upsert({
    where: { name },
    update: { name, value: String(value) },
    create: { name, value: String(value) }
  })
}
