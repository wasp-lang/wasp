import { prisma } from 'wasp/server'

import { getSerializedObjects } from '../../../../../src/features/operations/queries'


export default async function (args: any, context: any) {
  return (getSerializedObjects as any)(args, {
    ...context,
    entities: {
    },
  })
}
