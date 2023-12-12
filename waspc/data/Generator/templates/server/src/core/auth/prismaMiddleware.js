{{={= =}=}}
import { hashPassword } from '../auth.js'
import { PASSWORD_FIELD } from '../../auth/validation.js'

let isPasswordHashingEnabled = true

export async function withDisabledPasswordHashing(action) {
  isPasswordHashingEnabled = false
  await action()
  isPasswordHashingEnabled = true
}

// async (params, next) => {
//   if (params.model !== '{= authEntityUpper =}') {
//     return next(params)
//   }

//   if (!isPasswordHashingEnabled) {
//     return next(params)
//   }

//   if (['create', 'update', 'updateMany'].includes(params.action)) {
//     if (params.args.data.hasOwnProperty(PASSWORD_FIELD)) {
//       params.args.data[PASSWORD_FIELD] = await hashPassword(params.args.data[PASSWORD_FIELD])
//     }
//   } else if (params.action === 'upsert') {
//     if (params.args.create.hasOwnProperty(PASSWORD_FIELD)) {
//       params.args.create[PASSWORD_FIELD] =
//         await hashPassword(params.args.create[PASSWORD_FIELD])
//     }
//     if (params.args.update.hasOwnProperty(PASSWORD_FIELD)) {
//       params.args.update[PASSWORD_FIELD] =
//         await hashPassword(params.args.update[PASSWORD_FIELD])
//     }
//   }

//   return next(params)
// }

// Make sure password is always hashed before storing to the database.
// TODO: add support for JSON serialization and deserialization
// of providerData field.
const registerPasswordHashing = (prismaClient) => {
  return prismaClient.$extends({
    query: {
      {= authEntityLower =}: {
        async create({ model, operation, args, query }) {
          return query(args);
        },
        async update({ model, operation, args, query }) {
          return query(args);
        },
        async updateMany({ model, operation, args, query }) {
          return query(args);
        },
        async upsert({ model, operation, args, query }) {
          return query(args);
        },
      },
    },
  })
}

export const registerAuthMiddleware = (prismaClient) => {
  let extendedPrismaClient = registerPasswordHashing(prismaClient)

  return extendedPrismaClient
}
