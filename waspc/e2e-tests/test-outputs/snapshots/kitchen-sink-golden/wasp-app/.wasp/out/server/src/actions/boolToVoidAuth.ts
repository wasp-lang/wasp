import { prisma } from 'wasp/server'

import { boolToVoidAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args: any, context: any) {
  return (boolToVoidAuth as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
