import HttpError from '@wasp/core/HttpError.js'


export const getTasks = async (args, context) => {
  const Task = context.entities.Task
  /*
  if (Math.random() < 0.5) {
    throw new HttpError(400, 'Random error: getting tasks failed.')
  }
  */

  const tasks = await Task.findMany({})

  return tasks
}
