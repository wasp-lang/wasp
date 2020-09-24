import HttpError from '@wasp/core/HttpError.js'
import Prisma from '@prisma/client'

const prisma = new Prisma.PrismaClient()

export const createTask = async (task, context) => {
  if (Math.random() < 0.5) {
    throw new HttpError(400, 'Failed to create task, random error!')
  }

  const newTask = await prisma.task.create({
    data: {
      description: task.description
    }
  })
}
