import HttpError from '@wasp/core/HttpError.js'
import { Task } from '@wasp/entities'
import {
  GetFoo,
  GetNumTasks,
  GetTask,
  GetTasks
} from '@wasp/queries/types'

export const getTasks: GetTasks<Task[]> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }
  console.log('user who made the query: ', context.user)
  console.log('TEST_ENV_VAR', process.env.TEST_ENV_VAR)

  const Task = context.entities.Task
  const tasks = await Task.findMany(
    {
      where: { user: { id: context.user.id } },
      orderBy: { id: 'asc' },
    }
  )
  return tasks
}

export const getFoo: GetFoo<number> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }
  const x = await context.entities.Foo.count()
  const y = await context.entities.Task.count()
  return x + y
}

export const getNumTasks: GetNumTasks<number> = async (args, context) => {
  return context.entities.Task.count()
}

export const getTask: GetTask<Task> = async ({ id }, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  const Task = context.entities.Task
  // NOTE(matija): we can't call findUnique() with the specific user, so we have to fetch user first
  // and then manually check.
  const task = await Task.findUnique({ where: { id }, include: { user: true } })
  if (!task) {
    throw new HttpError(404)
  }
  // 404 is used to 'hide' the current existence of a forbidden target resource as a security measure
  // for vulnerabilities like IDOR
  if (task.user.id !== context.user.id) {
    throw new HttpError(404)
  }

  return task
}
