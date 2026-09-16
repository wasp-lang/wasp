import { hashPassword } from "@wasp.sh/auth/server";
import { type AuthUser } from "wasp/auth";
import { type DbSeedFn, type PrismaClient } from "wasp/server";
import { getIdentityStore } from "wasp/server/auth";
import { createTask } from "../operations/actions.js";

async function createUser(_prismaClient: PrismaClient, data: any) {
  // The same identity store Wasp's own signup flow uses -- no raw table
  // access needed. Hashing stays the caller's explicit job.
  const createdUser = await getIdentityStore("wasp:username").createIdentity(
    data.username,
    {
      secrets: {
        hashedPassword: await hashPassword(data.password),
      },
    },
  );

  return {
    id: createdUser.id,
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
