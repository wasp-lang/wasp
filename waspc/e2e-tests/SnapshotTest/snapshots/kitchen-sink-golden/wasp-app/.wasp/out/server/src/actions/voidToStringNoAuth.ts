import { prisma } from 'wasp/server'

import { voidToStringNoAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args, context) {
  return (voidToStringNoAuth as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
