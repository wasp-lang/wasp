import { prisma } from 'wasp/server'

import { boolToVoidNoAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args, context) {
  return (boolToVoidNoAuth as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
