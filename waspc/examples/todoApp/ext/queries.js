import HttpError from '@wasp/core/HttpError.js'
import Prisma from '@prisma/client'

// TODO(matija): is it ok to create a new instance in every file? See if it needs to be a
// singleton somewhere, if it isn't already.
const prisma = new Prisma.PrismaClient()

export const getTasks = async (args, context) => {
  /*
  if (Math.random() < 0.5) {
    throw new HttpError(400, 'Random error: getting tasks failed.')
  }
  */

  const tasks = await prisma.task.findMany({})

  return tasks
}

export const getTask = async ({ id }, context) => {
  const task = await prisma.task.findOne({ where: { id } })

  return task
}
