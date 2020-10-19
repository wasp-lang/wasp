import HttpError from '@wasp/core/HttpError.js'

export const getTasks = async (args, context) => {
  if (!context.user) {
    throw new HttpError(403)
  }
  console.log('user who made the query: ', context.user)

  const Task = context.entities.Task
  const tasks = await Task.findMany(
    { where: { user: { id: context.user.id } } }
  )
  return tasks
}

export const getTask = async ({ id }, context) => {
  if (!context.user) {
    throw new HttpError(403)
  }

  const Task = context.entities.Task
  const task = await Task.findOne(
    { where: { id, user: { id: context.user.id } } }
  )
  return task
}
