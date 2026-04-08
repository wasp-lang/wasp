import { prisma } from 'wasp/server'

import { updateTaskIsDone } from '../../../../../src/features/operations/actions'


export default async function (args, context) {
  return (updateTaskIsDone as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
