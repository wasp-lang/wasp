import { createCrudOverrides } from "@wasp/crud/tasks.js";

export const overrides = createCrudOverrides({
  Get(args, user) {
    return {
      include: {
        user: {
          select: {
            username: true,
          },
        },
      },
    };
  },
  GetAll: () => {
    return {
      orderBy: { id: "desc" },
      select: {
        id: true,
        title: true,
        user: {
          select: {
            username: true,
          },
        },
      },
    };
  },
  Create(args, user) {
    return {
      data: {
        title: args.title!,
        user: {
          connect: {
            id: user.id,
          },
        },
      },
    };
  },
});
