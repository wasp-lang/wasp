import { prisma } from 'wasp/server'

import { getAdminReport } from '../../../../../src/operations'


export default async function (args, context) {
  return (getAdminReport as any)(args, {
    ...context,
    entities: {
      Task: prisma.task,
    },
  })
}
