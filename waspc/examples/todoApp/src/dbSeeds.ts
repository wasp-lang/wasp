import { PrismaClient } from '@prisma/client/index.js'
import { type DbSeedFn } from 'wasp/server'
import { sanitizeAndSerializeProviderData } from 'wasp/server/auth'
import { createTask } from './actions.js'
import { createAuthUser } from 'wasp/auth/user.js'

async function createUser(prismaClient: PrismaClient, data: any) {
  const newUser = await prismaClient.user.create({
    data: {
      auth: {
        create: {
          identities: {
            create: {
              providerName: 'username',
              providerUserId: data.username,
              providerData: await sanitizeAndSerializeProviderData<'username'>({
                hashedPassword: data.password,
              }),
            },
          },
        },
      },
    },
    include: {
      auth: {
        select: {
          id: true,
          userId: true,
          identities: true,
        },
        include: {
          identities: true,
        },
      },
    },
  })

  return createAuthUser(newUser as any)
}

export const devSeedSimple: DbSeedFn = async (prismaClient) => {
  const user = await createUser(prismaClient, {
    username: 'martinsos',
    password: 'test1234',
  })

  await createTask(
    { description: 'My initial task' },
    { user, entities: { Task: prismaClient.task } }
  )

  console.log('Did simple dev seed!')
}

export const prodSeed: DbSeedFn = async (prismaClient) => {
  const user = await createUser(prismaClient, {
    username: 'martinsosProd',
    password: 'test1234prod',
  })

  await createTask(
    { description: 'My initial task in production' },
    { user, entities: { Task: prismaClient.task } }
  )

  console.log('Did seeding intended for production!')
}
