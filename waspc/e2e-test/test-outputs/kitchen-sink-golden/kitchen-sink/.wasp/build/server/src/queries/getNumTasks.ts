import { prisma } from 'wasp/server'

import { getNumTasks } from '../../../../../src/features/operations/queries'


export default async function (args, context) {
  return (getNumTasks as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
