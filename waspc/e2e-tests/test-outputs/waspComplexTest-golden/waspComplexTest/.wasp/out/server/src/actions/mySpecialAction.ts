import { prisma } from 'wasp/server'

import { foo } from '../../../../../src/server/actions/bar.js'


export default async function (args, context) {
  return (foo as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}
