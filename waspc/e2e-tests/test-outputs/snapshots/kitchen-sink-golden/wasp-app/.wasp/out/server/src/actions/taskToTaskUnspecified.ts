import { prisma } from 'wasp/server'

import { taskToTaskUnspecified } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args: any, context: any) {
  return (taskToTaskUnspecified as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
