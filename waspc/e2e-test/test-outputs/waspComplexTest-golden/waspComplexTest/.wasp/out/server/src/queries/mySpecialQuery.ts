import { prisma } from 'wasp/server'

import { foo as __userDefinedFoo } from '../../../../../src/server/queries/bar.js'


export default async function (args, context) {
  return (__userDefinedFoo as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}

export type MySpecialQuery = typeof __userDefinedFoo 
