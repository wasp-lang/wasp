import { prisma } from 'wasp/server'

import { foo as __userDefinedFoo } from 'wasp/ext-src/server/actions/bar.js'

// PRIVATE API
export type MySpecialAction = typeof __userDefinedFoo 

// PUBLIC API
export const mySpecialAction = async (args, context) => {
  return (__userDefinedFoo as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}
