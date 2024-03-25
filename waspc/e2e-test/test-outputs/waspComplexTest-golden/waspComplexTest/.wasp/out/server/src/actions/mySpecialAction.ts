import { prisma } from 'wasp/server'

import { foo as __userDefinedFoo } from '../../../../../src/server/actions/bar.js'


export default async function (args, context) {
  return (__userDefinedFoo as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}

export type MySpecialAction = typeof __userDefinedFoo 
