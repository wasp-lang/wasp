import HttpError from '@wasp/core/HttpError.js'
import { createNewUser } from '@wasp/core/auth.js'

export const createTask = async ({ description }, context) => {
  if (!context.user) { throw new HttpError(403) }
  return context.entities.Task.create({
    data: {
      description,
      user: { connect: { id: context.user.id } }
    }
  })
}

export const updateTask = async ({ taskId, data }, context) => {
  if (!context.user) { throw new HttpError(403) }
  return context.entities.Task.updateMany({
    where: { id: taskId, user: { id: context.user.id } },
    data: {
      isDone: data.isDone
    }
  })
}

export const signUp = async ({ email, password }, context) => {
  await createNewUser({ email, password })
}
