import { prisma } from 'wasp/server'

import { getDate } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args, context) {
  return (getDate as any)(args, {
    ...context,
    entities: {
    },
  })
}
