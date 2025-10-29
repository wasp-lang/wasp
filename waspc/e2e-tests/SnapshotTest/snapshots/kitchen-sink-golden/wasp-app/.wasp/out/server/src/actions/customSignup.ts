import { prisma } from 'wasp/server'

import { signup } from '../../../../../src/features/auth/customSignup'


export default async function (args, context) {
  return (signup as any)(args, {
    ...context,
    entities: {
    },
  })
}
