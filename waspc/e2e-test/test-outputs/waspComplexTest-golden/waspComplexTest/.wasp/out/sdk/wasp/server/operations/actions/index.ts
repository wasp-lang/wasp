import { prisma } from 'wasp/server'

import { foo as foo_ext } from 'wasp/ext-src/server/actions/bar'

// PRIVATE API
export type MySpecialAction = typeof foo_ext 

// PUBLIC API
export const mySpecialAction = async (args, context) => {
  return (foo_ext as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}
