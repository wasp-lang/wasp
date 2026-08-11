import { prisma } from 'wasp/server'

import { getTextUppercaseRequests } from '../../../../../src/features/jobs/uppercaseText'


export default async function (args: any, context: any) {
  return (getTextUppercaseRequests as any)(args, {
    ...context,
    entities: {
      UppercaseTextRequest: prisma.uppercaseTextRequest,
    },
  })
}
