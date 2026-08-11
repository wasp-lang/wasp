import { prisma } from 'wasp/server'

import { getAnyAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args: any, context: any) {
  return (getAnyAuth as any)(args, {
    ...context,
    entities: {
    },
  })
}
