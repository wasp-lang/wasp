{{={= =}=}}
import Prisma from '@prisma/client'

{=& jsFnImportStatement =}

{=! TODO: This template is exactly the same at the moment as one for queries,
          consider in the future if it is worth removing this duplication. =}

const prisma = new Prisma.PrismaClient()

export default async (args, context) => {
  context = { ...context, entities: {
    {=# entities =}
    {= name =}: prisma.{= prismaIdentifier =}
    {=/ entities =}
  }}
  return {= jsFnIdentifier =}(args, context)
}
