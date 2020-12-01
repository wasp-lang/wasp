{{={= =}=}}
import Prisma from '@prisma/client'

import { hashPassword } from './core/auth.js'

{=# isAuthEnabled =}
const PASSWORD_FIELD = 'password'

{=/ isAuthEnabled =}
const createDbClient = () => {
  const prismaClient = new Prisma.PrismaClient()

  {=# isAuthEnabled =}
  prismaClient.$use(async (params, next) => {
    // Make sure password is always hashed before storing to the database.
    if (params.model === '{= userEntityUpper =}') {
      if (['create', 'update', 'updateMany'].includes(params.action)) {
        if (params.args.data.hasOwnProperty(PASSWORD_FIELD)) {
          params.args.data[PASSWORD_FIELD] = await hashPassword(params.args.data[PASSWORD_FIELD])
        }
      } else if (params.action === 'upsert') {
        if (params.args.create.data.hasOwnProperty(PASSWORD_FIELD)) {
          params.args.create.data[PASSWORD_FIELD] =
            await hashPassword(params.args.create.data[PASSWORD_FIELD])
        }
        if (params.args.update.data.hasOwnProperty(PASSWORD_FIELD)) {
          params.args.update.data[PASSWORD_FIELD] =
            await hashPassword(params.args.update.data[PASSWORD_FIELD])
        }
      }
    }

    const result = next(params)

    return result
  })

  {=/ isAuthEnabled =}
  return prismaClient
}

const dbClient = createDbClient()

export default dbClient
