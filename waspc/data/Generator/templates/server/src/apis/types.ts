{{={= =}=}}

import prisma from '../dbClient.js'

{=# isAuthEnabled =}
import { {= userEntityUpper =} } from '../entities'
{=/ isAuthEnabled =}

{=# apiRoutes =}
export type {= name =}Context = {
  {=# isUsingAuth =}
  user: {= userEntityUpper =},
  {=/ isUsingAuth =}
  entities: {
    {=# entities =}
    {= name =}: typeof prisma.{= prismaIdentifier =},
    {=/ entities =}
  }
}
{=/ apiRoutes =}
