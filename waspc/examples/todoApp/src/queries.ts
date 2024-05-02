import { type Task } from 'wasp/entities'
import { HttpError } from 'wasp/server'
import type {
  GetNumTasks,
  GetTask,
  GetTasks,
  GetDate,
  GetAnything,
  GetTrueVoid,
} from 'wasp/server/operations'

export const getTasks: GetTasks<void, Task[]> = async (_args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }
  console.log('user who made the query: ', context.user)
  console.log('TEST_ENV_VAR', process.env.TEST_ENV_VAR)

  const Task = context.entities.Task
  const tasks = await Task.findMany({
    where: { user: { id: context.user.id } },
    orderBy: { id: 'asc' },
  })
  return tasks
}

export const getNumTasks: GetNumTasks<void, number> = async (
  _args,
  context
) => {
  return context.entities.Task.count()
}

export const getTask: GetTask<Pick<Task, 'id'>, Task> = async (
  where,
  context
) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  const Task = context.entities.Task
  // NOTE(matija): we can't call findUnique() with the specific user, so we have to fetch user first
  // and then manually check.
  const task = await Task.findUnique({ where, include: { user: true } })
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

export const getDate: GetDate<void, Date> = async () => {
  return new Date()
}

export const getAnything: GetAnything = async () => {
  return 'anything'
}

export const getTrueVoid = (async () => {
  return 'anything'
}) satisfies GetTrueVoid
