import { prisma } from 'wasp/server'

import { getTasks } from '../../../../../src/features/operations/queries'


export default async function (args: any, context: any) {
  return (getTasks as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
