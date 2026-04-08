import { prisma } from 'wasp/server'

import { getAnythingAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args, context) {
  return (getAnythingAuth as any)(args, {
    ...context,
    entities: {
    },
  })
}
