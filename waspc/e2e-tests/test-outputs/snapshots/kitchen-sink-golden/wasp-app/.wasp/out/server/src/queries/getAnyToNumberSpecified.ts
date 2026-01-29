import { prisma } from 'wasp/server'

import { getAnyToNumberSpecified } from '../../../../../src/rpcTests/operations/definitions'


export default async function (args, context) {
  return (getAnyToNumberSpecified as any)(args, {
    ...context,
    entities: {
    },
  })
}
