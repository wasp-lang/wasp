import { DbSeedFn } from 'wasp/server';

export const seedMyDb: DbSeedFn = async (prisma) => {
  const user = await prisma.user.findFirst({});

  if (!user) {
    console.log("no user found");
    return;
  }

  await prisma.task.create({
    data: {
      description: "My first task",
      user: {
        connect: {
          id: user.id,
        },
      },
    },
  });
};
