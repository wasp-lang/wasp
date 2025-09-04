import { prisma } from 'wasp/server'

import { createTag } from '../../../../../src/tags/actions'


export default async function (args, context) {
  return (createTag as any)(args, {
    ...context,
    entities: {
      Tag: prisma.tag,
    },
  })
}
