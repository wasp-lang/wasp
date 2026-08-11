import { prisma } from 'wasp/server'

import { getDate } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args: any, context: any) {
  return (getDate as any)(args, {
    ...context,
    entities: {
    },
  })
}
