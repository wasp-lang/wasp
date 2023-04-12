import prisma from '../dbClient.js'

import { foo } from '../ext-src/actions/bar.js'


export default async function (args, context) {
  return (foo as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}

export type MySpecialAction = typeof foo 
