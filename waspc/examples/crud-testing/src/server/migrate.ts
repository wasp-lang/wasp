import prisma from '@wasp/dbClient.js'
import { withDisabledPasswordHashing } from '@wasp/core/auth/prismaMiddleware.js'

export async function migrateAuth(db: typeof prisma) {
  const users = await db.user.findMany()

  for (let user of users) {
    const auth = await db.auth.findUnique({
      where: {
        username: user.username,
      },
    })
    if (auth) {
      continue
    }
    withDisabledPasswordHashing(() =>
      db.auth.create({
        data: {
          username: user.username,
          password: user.password,
          user: {
            connect: {
              id: user.id,
            },
          },
        },
      })
    )
  }
}
