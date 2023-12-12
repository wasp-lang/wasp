{{={= =}=}}
let isPasswordHashingEnabled = true

export async function withDisabledPasswordHashing(action) {
  isPasswordHashingEnabled = false
  await action()
  isPasswordHashingEnabled = true
}

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
