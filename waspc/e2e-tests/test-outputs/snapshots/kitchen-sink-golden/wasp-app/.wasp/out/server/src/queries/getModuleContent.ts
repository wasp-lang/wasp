import { prisma } from 'wasp/server'

import { getModuleContent } from '@kitchen-sink/module/queries'


export default async function (args, context) {
  return (getModuleContent as any)(args, {
    ...context,
    entities: {
    },
  })
}
