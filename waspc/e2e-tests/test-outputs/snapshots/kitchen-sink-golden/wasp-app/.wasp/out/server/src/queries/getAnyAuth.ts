import { prisma } from 'wasp/server'

import { getAnyAuth } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args, context) {
  return (getAnyAuth as any)(args, {
    ...context,
    entities: {
    },
  })
}
