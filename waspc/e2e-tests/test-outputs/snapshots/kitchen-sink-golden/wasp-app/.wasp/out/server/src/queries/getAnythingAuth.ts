import { prisma } from 'wasp/server'

import { getAnythingAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args: any, context: any) {
  return (getAnythingAuth as any)(args, {
    ...context,
    entities: {
    },
  })
}
