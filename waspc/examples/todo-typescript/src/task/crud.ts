import { Task } from 'wasp/entities'
import { Tasks } from 'wasp/server/crud'

export const getAllQuery = ((args, context) => {
  return context.entities.Task.findMany({})
}) satisfies Tasks.GetAllQuery<{}, Task[]>
