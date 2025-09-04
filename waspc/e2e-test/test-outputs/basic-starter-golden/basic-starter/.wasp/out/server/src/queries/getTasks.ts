import { prisma } from 'wasp/server'

import { getTasks } from '../../../../../src/tasks/queries'


export default async function (args, context) {
  return (getTasks as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
      Tag: prisma.tag,
    },
  })
}
