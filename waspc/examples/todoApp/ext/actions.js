import HttpError from '@wasp/core/HttpError.js'

export const createTask = async (task, context) => {
  const Task = context.entities.Task
  /*
  if (Math.random() < 0.5) {
    throw new HttpError(400, 'Failed to create task, random error!')
  }
  */

  return Task.create({
    data: {
      description: task.description
    }
  })
}

export const updateTaskIsDone = async ({ taskId, newIsDoneVal }, context) => {
  const Task = context.entities.Task
  return Task.update({
    where: { id: taskId },
    data: { isDone: newIsDoneVal }
  })
}

export const deleteCompletedTasks = async (args, context) => {
  const Task = context.entities.Task
  await Task.deleteMany({
    where: { isDone: true }
  })
}

export const toggleAllTasks = async (args, context) => {
  const Task = context.entities.Task
  const notDoneTasksCount = await Task.count({ where: { isDone: false } })

  if (notDoneTasksCount > 0) {
    await Task.updateMany({ where: { isDone: false }, data: { isDone: true } })
  } else {
    await Task.updateMany({ data: { isDone: false } })
  }
}
