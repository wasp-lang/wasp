import { prisma } from 'wasp/server'

import { createTask } from '../../../../../src/features/operations/actions'


export default async function (args, context) {
  return (createTask as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
