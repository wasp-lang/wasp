import { prisma } from 'wasp/server'

import { voidToStringAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args: any, context: any) {
  return (voidToStringAuth as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
