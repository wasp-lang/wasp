import { Task } from 'wasp/entities'
import { HttpError } from 'wasp/server'
import { Tasks } from 'wasp/server/crud'

export const getAllFilteredTasks: Tasks.GetAllQuery<
  { filter: string } | void,
  Task[]
> = (args, context) => {
  if (!context.user) {
    throw new HttpError(401, 'Unauthorized')
  }
  return context.entities.Task.findMany({
    where: {
      description: {
        contains: args ? args.filter : '',
      },
      user: {
        id: context.user.id,
      },
    },
  })
}
