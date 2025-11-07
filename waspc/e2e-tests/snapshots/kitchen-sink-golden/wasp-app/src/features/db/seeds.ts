import { type AuthUser } from "wasp/auth";
import { type DbSeedFn, type PrismaClient } from "wasp/server";
import { sanitizeAndSerializeProviderData } from "wasp/server/auth";
import { createTask } from "../operations/actions.js";

async function createUser(prismaClient: PrismaClient, data: any) {
  const newUser = await prismaClient.user.create({
    data: {
      auth: {
        create: {
          identities: {
            create: {
              providerName: "username",
              providerUserId: data.username,
              providerData: await sanitizeAndSerializeProviderData<"username">({
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
  });

  return {
    id: newUser.id,
  } as AuthUser;
}

export const devSeedSimple: DbSeedFn = async (prismaClient) => {
  const user = await createUser(prismaClient, {
    username: "martinsos",
    password: "test1234",
  });

  await createTask(
    { description: "My initial task" },
    { user, entities: { Task: prismaClient.task } },
  );

  console.log("Did simple dev seed!");
};

export const prodSeed: DbSeedFn = async (prismaClient) => {
  const user = await createUser(prismaClient, {
    username: "martinsosProd",
    password: "test1234prod",
  });

  await createTask(
    { description: "My initial task in production" },
    { user, entities: { Task: prismaClient.task } },
  );

  console.log("Did seeding intended for production!");
};
