import HttpError from '@wasp/core/HttpError.js'

export const createTask = async (args, context) => {
  if (!context.user) { throw new HttpError(401) }
  return context.entities.Task.create({
    data: {
      description: args.description,
      user: { connect: { id: context.user.id } }
    }
  })
}

export const updateTask = async (args, context) => {
  if (!context.user) { throw new HttpError(401) }
  return context.entities.Task.updateMany({
    where: { id: args.taskId, user: { id: context.user.id } },
    data: { isDone: args.data.isDone }
  })
}

