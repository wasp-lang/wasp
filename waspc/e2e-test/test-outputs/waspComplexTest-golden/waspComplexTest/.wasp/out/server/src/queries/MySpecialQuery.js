import prisma from '../dbClient.js'

import { foo } from '../ext-src/queries/bar.js'


export default async function (args, context) {
  return foo(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}
