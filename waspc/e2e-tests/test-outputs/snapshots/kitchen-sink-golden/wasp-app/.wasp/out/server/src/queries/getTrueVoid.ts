import { prisma } from 'wasp/server'

import { getTrueVoid } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args: any, context: any) {
  return (getTrueVoid as any)(args, {
    ...context,
    entities: {
    },
  })
}
