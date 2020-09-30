import HttpError from '@wasp/core/HttpError.js'
import Prisma from '@prisma/client'

const prisma = new Prisma.PrismaClient()

export const createTask = async (task, context) => {
  /*
  if (Math.random() < 0.5) {
    throw new HttpError(400, 'Failed to create task, random error!')
  }
  */

  const newTask = await prisma.task.create({
    data: {
      description: task.description
    }
  })
}

export const updateTaskIsDone = async ({taskId, newIsDoneVal}, context) => {
  await prisma.task.update({
    where: { id: taskId },
    data: { isDone: newIsDoneVal }
  })
}

export const deleteCompletedTasks = async () => {
  await prisma.task.deleteMany({
    where: { isDone: true }
  })
}

export const toggleAllTasks = async () => {
  const notDoneTasksCount = await prisma.task.count({ where: { isDone: false } })

  if (notDoneTasksCount > 0) {
    await prisma.task.updateMany({ where: { isDone: false }, data: { isDone: true } })
  } else {
    await prisma.task.updateMany({ data: { isDone: false } })
  }
}
