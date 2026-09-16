import { prisma } from 'wasp/server'

import { getMyTasks } from '../../../../../src/operations'


export default async function (args, context) {
  return (getMyTasks as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
