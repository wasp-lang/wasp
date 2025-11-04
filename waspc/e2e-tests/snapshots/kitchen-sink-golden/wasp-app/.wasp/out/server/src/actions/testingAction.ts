import { prisma } from 'wasp/server'

import { testingAction } from '../../../../../src/rpcTests/operations/server'


export default async function (args, context) {
  return (testingAction as any)(args, {
    ...context,
    entities: {
    },
  })
}
