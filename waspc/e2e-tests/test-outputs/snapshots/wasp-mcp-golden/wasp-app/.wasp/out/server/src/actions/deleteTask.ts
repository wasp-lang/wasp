import { prisma } from 'wasp/server'

import { deleteTask } from "../../../../../src/tasks/operations"


export default async function (args, context) {
  return (deleteTask as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
