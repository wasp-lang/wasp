import { prisma } from 'wasp/server'

import { getTags } from '../../../../../src/tags/queries'


export default async function (args, context) {
  return (getTags as any)(args, {
    ...context,
    entities: {
      Tag: prisma.tag,
    },
  })
}
