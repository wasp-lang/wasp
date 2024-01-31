{{={= =}=}}
import { prisma } from 'wasp/server'
{=! TODO: This template is exactly the same at the moment as one for queries,
          consider in the future if it is worth removing this duplication. =}

{=! TODO: This will generate multiple import statements even though they're
          importing symbols from the same file. We should improve our importing machinery
          to support multiple imports from the same file =}
{=# operations =}
{=& jsFn.importStatement =}
{=/ operations =}
{=# operations =}

// PRIVATE API
export type {= operationTypeName =} = typeof {= jsFn.importIdentifier =} 

// PUBLIC API
export const {= operationName =} = async (args, context) => {
  return ({= jsFn.importIdentifier =} as any)(args, {
    ...context,
    entities: {
      {=# entities =}
      {= name =}: prisma.{= prismaIdentifier =},
      {=/ entities =}
    },
  })
}
{=/ operations =}
