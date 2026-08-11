import { prisma } from 'wasp/server'

import { jsActionWithArgs } from '../../../../../src/rpcTests/operations/jsDefinitions'


export default async function (args: any, context: any) {
  return (jsActionWithArgs as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
