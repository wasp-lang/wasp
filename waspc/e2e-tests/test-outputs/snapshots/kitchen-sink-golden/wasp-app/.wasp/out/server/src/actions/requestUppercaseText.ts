import { prisma } from 'wasp/server'

import { requestUppercaseText } from '../../../../../src/features/jobs/uppercaseText'


export default async function (args, context) {
  return (requestUppercaseText as any)(args, {
    ...context,
    entities: {
      UppercaseTextRequest: prisma.uppercaseTextRequest,
    },
  })
}
