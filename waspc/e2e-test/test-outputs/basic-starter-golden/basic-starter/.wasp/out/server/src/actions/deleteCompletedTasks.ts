import { prisma } from 'wasp/server'

import { deleteCompletedTasks } from '../../../../../src/tasks/actions'


export default async function (args, context) {
  return (deleteCompletedTasks as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
