import { prisma } from 'wasp/server'

import { boolToStringNoAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args: any, context: any) {
  return (boolToStringNoAuth as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
