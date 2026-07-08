import { prisma } from 'wasp/server'

import { getTodoItems } from '@kitchen-sink/module/queries'


export default async function (args, context) {
  return (getTodoItems as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
