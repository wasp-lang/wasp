import { prisma } from 'wasp/server'

import { testingAction } from '../../../../../src/rpcTests/operations/server'


export default async function (args: any, context: any) {
  return (testingAction as any)(args, {
    ...context,
    entities: {
    },
  })
}
