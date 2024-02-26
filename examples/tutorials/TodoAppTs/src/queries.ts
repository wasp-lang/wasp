import { Task } from 'wasp/entities'
import { HttpError } from 'wasp/server'
import { GetTasks } from 'wasp/server/operations'

export const getTasks: GetTasks<void, Task[]> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }
  return context.entities.Task.findMany({
    where: { user: { id: context.user.id } },
    orderBy: { id: 'asc' },
  })
}
