import { prisma } from 'wasp/server'

import { foo as __userDefinedFoo } from 'wasp/ext-src/server/queries/bar.js'

// PRIVATE API
export type MySpecialQuery = typeof __userDefinedFoo 

// PUBLIC API
export const mySpecialQuery = async (args, context) => {
  return (__userDefinedFoo as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}
