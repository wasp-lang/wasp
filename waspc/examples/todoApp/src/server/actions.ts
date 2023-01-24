import HttpError from '@wasp/core/HttpError.js'
import { Task } from '@wasp/entities/'
import { AuthenticatedAction } from '@wasp/types'
import { getSomeResource } from './serverSetup.js'

export const createTask: AuthenticatedAction<[Task]> = async (task, context) => {
  if (!context.user) {
    throw new HttpError(401)
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

export const updateTaskIsDone: AuthenticatedAction<[Task]> = async ({ id, isDone }, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  // Uncomment to test optimistic updates
  // const sleep = (ms) => new Promise(res => setTimeout(res, ms))
  // await sleep(3000);

  const Task = context.entities.Task
  return Task.updateMany({
    where: { id, user: { id: context.user.id } },
    data: { isDone }
  })
}

export const deleteCompletedTasks: AuthenticatedAction<[Task]> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  const Task = context.entities.Task
  await Task.deleteMany({
    where: { isDone: true, user: { id: context.user.id } }
  })
}

export const toggleAllTasks: AuthenticatedAction<[Task]> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401)
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
