import { prisma } from 'wasp/server'

import { taskToTaskSpecified } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args, context) {
  return (taskToTaskSpecified as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
