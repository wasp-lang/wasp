import { prisma } from 'wasp/server'

import { foo as foo__userDefined } from 'wasp/ext-src/server/queries/bar.js'

// PRIVATE API
export type MySpecialQuery = typeof foo__userDefined 

// PUBLIC API
export const mySpecialQuery = async (args, context) => {
  return (foo__userDefined as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}
