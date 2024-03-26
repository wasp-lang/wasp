import { sanitizeAndSerializeProviderData } from "wasp/server/auth";
import { prisma } from "wasp/server";

export async function migrateAuth(db: typeof prisma) {
  // 0. Update to the latest version of Wasp and run `wasp db migrate-dev`
  // 1. Run this migration script
  // 2. Then remove the username & password fields from User model
  const users = await db.user.findMany()

  for (let user of users) {
    const username = (user as any).username
    const authIdentity = await db.authIdentity.findUnique({
      where: {
        providerName_providerUserId: {
          providerName: 'username',
          providerUserId: username,
        },
      },
    })

    // If authIdentity already exists, skip this user
    if (authIdentity) {
      continue
    }

    db.auth.create({
      data: {
        user: {
          connect: {
            id: user.id,
          },
        },
        identities: {
          create: {
            providerName: 'username',
            providerUserId: username,
            providerData: await sanitizeAndSerializeProviderData<'username'>({
              hashedPassword: (user as any).password,
            }),
          },
        },
      },
    })
  }
}
