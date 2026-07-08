import { prisma } from 'wasp/server'

import { addRandomTodo } from '@kitchen-sink/module/actions'


export default async function (args, context) {
  return (addRandomTodo as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
