import { prisma } from 'wasp/server'

import getOldestTask from '../../../../../src/features/operations/getOldestTask'


export default async function (args, context) {
  return (getOldestTask as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
