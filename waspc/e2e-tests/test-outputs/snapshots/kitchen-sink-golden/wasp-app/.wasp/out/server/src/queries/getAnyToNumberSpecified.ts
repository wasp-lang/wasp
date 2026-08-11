import { prisma } from 'wasp/server'

import { getAnyToNumberSpecified } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args: any, context: any) {
  return (getAnyToNumberSpecified as any)(args, {
    ...context,
    entities: {
    },
  })
}
