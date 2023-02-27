{{={= =}=}}

import prisma from '../dbClient.js'

{=# apiRoutes =}
export type {= name =}Context = {
  entities: {
    {=# entities =}
    {= name =}: typeof prisma.{= prismaIdentifier =},
    {=/ entities =}
  }
}
{=/ apiRoutes =}
