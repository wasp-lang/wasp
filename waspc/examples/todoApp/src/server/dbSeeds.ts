import { createTask } from './actions.js'
import type { DbSeedFn } from '@wasp/dbSeed/types.js'
import { PrismaClient } from '@prisma/client/index.js'
import { hashPassword } from '@wasp/core/auth.js'

async function createUser(prismaClient: PrismaClient, data: any) {
  const newUser = await prismaClient.user.create({
    data: {
      auth: {
        create: {
          identities: {
            create: {
              providerName: 'username',
              providerUserId: data.username,
              providerData: JSON.stringify({
                password: hashPassword(data.password),
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

  if (newUser.auth?.identities) {
    newUser.auth.identities = newUser.auth.identities.map((identity) => {
      identity.providerData = JSON.parse(identity.providerData)
      return identity
    })
  }

  return newUser
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
