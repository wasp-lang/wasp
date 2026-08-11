import { prisma } from 'wasp/server'

import { getAnythingNoAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args: any, context: any) {
  return (getAnythingNoAuth as any)(args, {
    ...context,
    entities: {
    },
  })
}
