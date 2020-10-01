{{={= =}=}}
import Prisma from '@prisma/client'

{=& jsFnImportStatement =}


const prisma = new Prisma.PrismaClient()

// TODO: Implement same thing for actions.

export default async (args, context) => {
  context = { ...context, entities: {
    {=# entities =}
    {= name =}: prisma.{= prismaIdentifier =}
    {=/ entities =}
  }}
  return {= jsFnIdentifier =}(args, context)
}
