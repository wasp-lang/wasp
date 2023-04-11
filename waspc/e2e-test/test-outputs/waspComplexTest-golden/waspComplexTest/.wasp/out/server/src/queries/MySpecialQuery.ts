import prisma from '../dbClient.js'

import { foo } from '../ext-src/queries/bar.js'


export default async function (args, context) {
  return (foo as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}

export type MySpecialQuery = typeof foo 
