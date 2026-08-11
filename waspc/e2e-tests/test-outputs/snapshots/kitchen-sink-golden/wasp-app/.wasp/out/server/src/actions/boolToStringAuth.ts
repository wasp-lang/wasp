import { prisma } from 'wasp/server'

import { boolToStringAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args: any, context: any) {
  return (boolToStringAuth as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
