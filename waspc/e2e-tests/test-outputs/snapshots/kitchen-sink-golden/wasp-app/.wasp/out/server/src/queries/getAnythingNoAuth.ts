import { prisma } from 'wasp/server'

import { getAnythingNoAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args, context) {
  return (getAnythingNoAuth as any)(args, {
    ...context,
    entities: {
    },
  })
}
