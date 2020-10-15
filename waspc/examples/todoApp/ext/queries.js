import HttpError from '@wasp/core/HttpError.js'

export const getTasks = async (args, context) => {
  if (!context.user) {
    throw new HttpError(403)
  }
  console.log('user who made the query: ', context.user)

  const Task = context.entities.Task
  /*
  if (Math.random() < 0.5) {
    throw new HttpError(400, 'Random error: getting tasks failed.')
  }
  */

  const tasks = await Task.findMany({})

  return tasks
}

export const getTask = async ({ id }, context) => {
  if (!context.user) {
    throw new HttpError(403)
  }

  const Task = context.entities.Task
  const task = await Task.findOne({ where: { id } })

  return task
}
