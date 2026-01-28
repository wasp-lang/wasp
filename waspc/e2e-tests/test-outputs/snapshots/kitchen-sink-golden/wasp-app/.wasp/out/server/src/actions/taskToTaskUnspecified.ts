import { prisma } from 'wasp/server'

import { taskToTaskUnspecified } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args, context) {
  return (taskToTaskUnspecified as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
