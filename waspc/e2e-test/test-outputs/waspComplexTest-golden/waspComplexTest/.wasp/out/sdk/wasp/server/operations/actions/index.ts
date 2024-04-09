import { prisma } from 'wasp/server'

import { foo as foo__userDefined } from 'wasp/ext-src/server/actions/bar.js'

// PRIVATE API
export type MySpecialAction = typeof foo__userDefined 

// PUBLIC API
export const mySpecialAction = async (args, context) => {
  return (foo__userDefined as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}
