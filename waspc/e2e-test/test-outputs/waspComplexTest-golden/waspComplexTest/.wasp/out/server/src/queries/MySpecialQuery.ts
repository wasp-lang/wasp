import { prisma } from 'wasp/server'

import { foo } from '../../../../../src/server/queries/bar.js'


export default async function (args, context) {
  return (foo as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}

export type MySpecialQuery = typeof foo 
