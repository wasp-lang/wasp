import HttpError from '@wasp/core/HttpError.js'

export const createTask = async ({ description }, context) => {
  if (!context.user) { throw new HttpError(401) }
  return context.entities.Task.create({
    data: {
      description,
      user: { connect: { id: context.user.id } }
    }
  })
}

export const updateTask = async ({ taskId, data }, context) => {
  if (!context.user) { throw new HttpError(401) }
  return context.entities.Task.updateMany({
    where: { id: taskId, user: { id: context.user.id } },
    data: {
      isDone: data.isDone
    }
  })
}
