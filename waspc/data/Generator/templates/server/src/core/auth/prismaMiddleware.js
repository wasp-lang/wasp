{{={= =}=}}
import { hashPassword } from '../auth.js'
import { PASSWORD_FIELD } from '../../auth/validation.js'

// Make sure password is always hashed before storing to the database.
const registerPasswordHashing = (prismaClient) => {
  prismaClient.$use(async (params, next) => {
    if (params.model === '{= userEntityUpper =}') {
      if (['create', 'update', 'updateMany'].includes(params.action)) {
        if (params.args.data.hasOwnProperty(PASSWORD_FIELD)) {
          params.args.data[PASSWORD_FIELD] = await hashPassword(params.args.data[PASSWORD_FIELD])
        }
      } else if (params.action === 'upsert') {
        if (params.args.create.hasOwnProperty(PASSWORD_FIELD)) {
          params.args.create[PASSWORD_FIELD] =
            await hashPassword(params.args.create[PASSWORD_FIELD])
        }
        if (params.args.update.hasOwnProperty(PASSWORD_FIELD)) {
          params.args.update[PASSWORD_FIELD] =
            await hashPassword(params.args.update[PASSWORD_FIELD])
        }
      }
    }

    return next(params)
  })
}

export const registerAuthMiddleware = (prismaClient) => {
  registerPasswordHashing(prismaClient)
}
