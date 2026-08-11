import { prisma } from 'wasp/server'

import { getTask } from '../../../../../src/features/operations/queries'


export default async function (args: any, context: any) {
  return (getTask as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
