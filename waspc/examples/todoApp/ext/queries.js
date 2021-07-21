import HttpError from '@wasp/core/HttpError.js'

export const getTasks = async (args, context) => {
  if (!context.user) {
    throw new HttpError(403)
  }
  console.log('user who made the query: ', context.user)
  console.log('TEST_ENV_VAR', process.env.TEST_ENV_VAR)

  const Task = context.entities.Task
  const tasks = await Task.findMany(
    { where: { user: { id: context.user.id } } }
  )
  return tasks
}

export const getNumTasks = async (args, context) => {
  return context.entities.Task.count()
}

export const getTask = async ({ id }, context) => {
  if (!context.user) {
    throw new HttpError(403)
  }

  const Task = context.entities.Task
  // NOTE(matija): we can't call findUnique() with the specific user, so we have to fetch user first
  // and then manually check.
  const task = await Task.findUnique({ where: { id }, include: { user: true } })
  if (!task) {
    throw new HttpError(404)
  }
  if (task.user.id !== context.user.id) {
    throw new HttpError(403)
  }

  return task
}
