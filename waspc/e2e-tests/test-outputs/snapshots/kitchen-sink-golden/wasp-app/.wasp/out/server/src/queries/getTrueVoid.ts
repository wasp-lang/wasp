import { prisma } from 'wasp/server'

import { getTrueVoid } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args, context) {
  return (getTrueVoid as any)(args, {
    ...context,
    entities: {
    },
  })
}
