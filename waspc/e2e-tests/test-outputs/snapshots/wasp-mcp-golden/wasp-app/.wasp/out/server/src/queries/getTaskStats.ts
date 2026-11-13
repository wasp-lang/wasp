import { prisma } from 'wasp/server'

import { getTaskStats } from "../../../../../src/tasks/operations"


export default async function (args, context) {
  return (getTaskStats as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
