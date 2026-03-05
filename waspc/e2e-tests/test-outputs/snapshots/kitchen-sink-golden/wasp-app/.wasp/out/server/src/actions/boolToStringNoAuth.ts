import { prisma } from 'wasp/server'

import { boolToStringNoAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args, context) {
  return (boolToStringNoAuth as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
