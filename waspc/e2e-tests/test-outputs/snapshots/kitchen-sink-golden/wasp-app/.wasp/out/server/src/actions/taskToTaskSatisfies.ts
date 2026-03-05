import { prisma } from 'wasp/server'

import { taskToTaskSatisfies } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args, context) {
  return (taskToTaskSatisfies as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
