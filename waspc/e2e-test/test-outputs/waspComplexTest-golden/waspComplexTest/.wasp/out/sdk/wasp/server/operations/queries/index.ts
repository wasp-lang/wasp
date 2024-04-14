import { prisma } from 'wasp/server'

import { foo as foo_ext } from 'wasp/ext-src/server/queries/bar'

// PRIVATE API
export type MySpecialQuery = typeof foo_ext 

// PUBLIC API
export const mySpecialQuery = async (args, context) => {
  return (foo_ext as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}
