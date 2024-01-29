{{={= =}=}}
import { prisma } from 'wasp/server'

{=& jsFn.importStatement =}

{=! TODO: This template is exactly the same at the moment as one for actions,
          consider in the future if it is worth removing this duplication. =}

export default async function (args, context) {
  return ({= jsFn.importIdentifier =} as any)(args, {
    ...context,
    entities: {
      {=# entities =}
      {= name =}: prisma.{= prismaIdentifier =},
      {=/ entities =}
    },
  })
}

export type {= operationTypeName =} = typeof {= jsFn.importIdentifier =} 
