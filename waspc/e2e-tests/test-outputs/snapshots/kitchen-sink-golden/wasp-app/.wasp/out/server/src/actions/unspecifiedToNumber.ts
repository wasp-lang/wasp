import { prisma } from 'wasp/server'

import { unspecifiedToNumber } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args: any, context: any) {
  return (unspecifiedToNumber as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
