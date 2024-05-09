import { type Task } from 'wasp/entities'
import { HttpError } from 'wasp/server'
import {
  type CreateTask,
  type DeleteCompletedTasks,
  type ToggleAllTasks,
  type UpdateTaskIsDone,
} from 'wasp/server/operations'
import { getSomeResource } from './serverSetup.js'

export const createTask: CreateTask<Pick<Task, 'description'>, Task> = async (
  task,
  context
) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  const Task = context.entities.Task

  const newTask = await Task.create({
    data: {
      description: task.description,
      user: {
        connect: { id: context.user.id },
      },
    },
  })

  console.log(
    'New task created! Btw, current value of someResource is: ' +
      getSomeResource()
  )

  return newTask
}

export const updateTaskIsDone: UpdateTaskIsDone<
  Pick<Task, 'id' | 'isDone'>
> = async ({ id, isDone }, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  // Uncomment to test optimistic updates
  const sleep = (ms: number) => new Promise((res) => setTimeout(res, ms))
  await sleep(2000)

  const Task = context.entities.Task
  const updateResult = await Task.updateMany({
    where: { id, user: { id: context.user.id } },
    data: { isDone },
  })
  return updateResult
}

export const deleteCompletedTasks: DeleteCompletedTasks = async (
  _args,
  context
) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  const Task = context.entities.Task
  await Task.deleteMany({
    where: { isDone: true, user: { id: context.user.id } },
  })
}

export const toggleAllTasks: ToggleAllTasks = async (_args, context) => {
  const user = context.user
  if (!user) {
    throw new HttpError(401)
  }

  const whereIsDone = (isDone: boolean) => ({
    isDone,
    user: { id: user.id },
  })
  const Task = context.entities.Task
  const notDoneTasksCount = await Task.count({ where: whereIsDone(false) })

  if (notDoneTasksCount > 0) {
    await Task.updateMany({ where: whereIsDone(false), data: { isDone: true } })
  } else {
    await Task.updateMany({ where: whereIsDone(true), data: { isDone: false } })
  }
}
