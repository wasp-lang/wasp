import HttpError from '@wasp/core/HttpError.js'

export const getTasks = async (args, context) => {
  if (!context.user) { throw new HttpError(403) }
  return context.entities.Task.findMany(
    { where: { user: { id: context.user.id } } }
  )
}
