import { prisma } from "wasp/server";
import { type ProviderName, type UsernameProviderData } from "wasp/server/auth";
import { MigrateUsernameAndPassword } from "wasp/server/api";

export const migrateUsernameAndPasswordHandler: MigrateUsernameAndPassword =
  async (_req, res) => {
    const result = await migrateUsernameAuth();

    res.status(200).json({ message: "Migrated users to the new auth", result });
  };

export async function migrateUsernameAuth(): Promise<{
  totalUsers: number;
  migratedUsers: number;
  skippedUsers: number;
}> {
  const result = {
    totalUsers: 0,
    migratedUsers: 0,
    skippedUsers: 0,
  };

  const users = await prisma.user.findMany({
    include: {
      auth: true,
    },
  });
  result.totalUsers = users.length;

  for (const user of users) {
    if (user.auth) {
      result.skippedUsers++;
      console.log("User was already migrated, skipping", user.username);
      continue;
    }

    if (!user.username || !user.password) {
      console.log("Missing username auth info, skipping user", user.username);
      continue;
    }

    const providerData: UsernameProviderData = {
      hashedPassword: user.password,
    };
    const providerName: ProviderName = "username";

    await prisma.auth.create({
      data: {
        identities: {
          create: {
            providerName,
            providerUserId: user.username.toLowerCase(),
            providerData: JSON.stringify(providerData),
          },
        },
        user: {
          connect: {
            id: user.id,
          },
        },
      },
    });
    result.migratedUsers++;
  }
  return result;
}
