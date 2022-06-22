import HttpError from '@wasp/core/HttpError.js'
import { getSomeResource } from './serverSetup.js'

const sleep = (ms) => new Promise(res => setTimeout(res, ms))

export const createTask = async (task, context) => {
  if (!context.user) {
    throw new HttpError(403)
  }

  const Task = context.entities.Task

  console.log('New task created! Btw, current value of someResource is: ' + getSomeResource())

  return Task.create({
    data: {
      description: task.description,
      user: {
        connect: { id: context.user.id }
      }
    }
  })
}

export const updateTaskIsDone = async ({ id, isDone }, context) => {
  if (!context.user) {
    throw new HttpError(403)
  }

  await sleep(3000);
  const Task = context.entities.Task
  return Task.updateMany({
    where: { id, user: { id: context.user.id } },
    data: { isDone }
  })
}

export const deleteCompletedTasks = async (args, context) => {
  if (!context.user) {
    throw new HttpError(403)
  }

  const Task = context.entities.Task
  await Task.deleteMany({
    where: { isDone: true, user: { id: context.user.id } }
  })
}

export const toggleAllTasks = async (args, context) => {
  if (!context.user) {
    throw new HttpError(403)
  }

  const whereIsDone = isDone => ({ isDone, user: { id: context.user.id } })
  const Task = context.entities.Task
  const notDoneTasksCount = await Task.count({ where: whereIsDone(false) })

  if (notDoneTasksCount > 0) {
    await Task.updateMany({ where: whereIsDone(false), data: { isDone: true } })
  } else {
    await Task.updateMany({ where: whereIsDone(true), data: { isDone: false } })
  }
}
