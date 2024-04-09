import { prisma } from 'wasp/server'

import { foo as foo__userDefined } from '../../../../../src/server/actions/bar.js'


export default async function (args, context) {
  return (foo__userDefined as any)(args, {
    ...context,
    entities: {
      User: prisma.user,
    },
  })
}

export type MySpecialAction = typeof foo__userDefined 
