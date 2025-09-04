import { prisma } from 'wasp/server'

import { updateTaskStatus } from '../../../../../src/tasks/actions'


export default async function (args, context) {
  return (updateTaskStatus as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
