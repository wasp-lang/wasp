import prisma from '@wasp/dbClient.js'
import { withDisabledPasswordHashing } from '@wasp/core/auth/prismaMiddleware.js'

export async function migrateAuth(db: typeof prisma) {
  const users = await db.user.findMany()

  // TODO: the User model needs to have username & password and then not
  // this is a tricky migration

  // for (let user of users) {
  //   const authIdentity = await db.authIdentity.findUnique({
  //     where: {
  //       providerName_providerUserId: {
  //         providerName: 'username',
  //         providerUserId: user.username,
  //       },
  //     },
  //   })
  //   if (authIdentity) {
  //     continue
  //   }
  //   withDisabledPasswordHashing(() =>
  //     db.auth.create({
  //       data: {
  //         user: {
  //           connect: {
  //             id: user.id,
  //           },
  //         },
  //         identities: {
  //           create: {
  //             providerName: 'username',
  //             providerUserId: user.username,
  //             providerData: JSON.stringify({
  //               password: user.password,
  //             }),
  //           },
  //         },
  //       },
  //     })
  //   )
  // }
}
