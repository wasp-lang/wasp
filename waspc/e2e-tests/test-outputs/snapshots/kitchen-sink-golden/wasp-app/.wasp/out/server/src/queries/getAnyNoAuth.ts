import { prisma } from 'wasp/server'

import { getAnyNoAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args: any, context: any) {
  return (getAnyNoAuth as any)(args, {
    ...context,
    entities: {
    },
  })
}
