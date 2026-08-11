import { prisma } from 'wasp/server'

import { deleteCompletedTasks } from '../../../../../src/features/operations/actions'


export default async function (args: any, context: any) {
  return (deleteCompletedTasks as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
