import { prisma } from 'wasp/server'

import { getTasks as getTasks_ext } from 'wasp/ext-src/task/queries.js'

export type GetTasks = typeof getTasks_ext 

export const getTasks = async (args, context) => {
  return (getTasks_ext as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
