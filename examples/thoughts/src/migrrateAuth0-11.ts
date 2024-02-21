import { PrismaClient } from "@prisma/client";
import { type ProviderName, type UsernameProviderData } from "wasp/server/auth";

export async function migrateUsernameAuth(prismaClient: PrismaClient) {
  const users = await prismaClient.user.findMany({
    include: {
      auth: true,
    },
  });

  for (const user of users) {
    if (user.auth) {
      console.log("User was already migrated, skipping", user);
      continue;
    } else {
      console.log("oh it wasn't migrated! Trouble trouble");
    }
  }
}