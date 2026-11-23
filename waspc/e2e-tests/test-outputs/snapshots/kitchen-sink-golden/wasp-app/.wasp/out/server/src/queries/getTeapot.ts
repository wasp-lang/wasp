import { prisma } from 'wasp/server'

import { getTeapot } from "../../../../../src/features/errors/queries"


export default async function (args, context) {
  return (getTeapot as any)(args, {
    ...context,
    entities: {
    },
  })
}
