import { prisma } from 'wasp/server'

import { getAnyNoAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args, context) {
  return (getAnyNoAuth as any)(args, {
    ...context,
    entities: {
    },
  })
}
