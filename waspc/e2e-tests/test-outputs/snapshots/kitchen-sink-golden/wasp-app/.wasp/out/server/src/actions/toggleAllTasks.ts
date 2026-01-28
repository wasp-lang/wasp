import { prisma } from 'wasp/server'

import { toggleAllTasks } from '../../../../../src/features/operations/actions'


export default async function (args, context) {
  return (toggleAllTasks as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
