import { prisma } from 'wasp/server'

import { toggleAllTasks } from '../../../../../src/features/operations/actions'


export default async function (args: any, context: any) {
  return (toggleAllTasks as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
