import { Task } from '@wasp/entities'
import { GetTasks } from '@wasp/queries/types'
import HttpError from '@wasp/core/HttpError.js'

export const getTasks: GetTasks<void, Task[]> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }
  return context.entities.Task.findMany({
    where: { user: { id: context.user.id } },
  })
}
