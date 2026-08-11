import { prisma } from 'wasp/server'

import { customSignup } from '../../../../../src/features/auth/customSignup'


export default async function (args: any, context: any) {
  return (customSignup as any)(args, {
    ...context,
    entities: {
    },
  })
}
