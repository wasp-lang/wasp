{{={= =}=}}
import prisma from '../dbClient.js'

{=& jsFnImportStatement =}

{=! TODO: This template is exactly the same at the moment as one for actions,
          consider in the future if it is worth removing this duplication. =}

export default async (args, context) => {
  context = { ...context, entities: {
    {=# entities =}
    {= name =}: prisma.{= prismaIdentifier =},
    {=/ entities =}
  }}
  return {= jsFnIdentifier =}(args, context)
}
