import { createCrudOverrides } from "@wasp/crud/tasks.js";

export const overrides = createCrudOverrides({
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
        ...args,
        user: {
          connect: {
            id: user.id,
          },
        },
      },
    };
  },
});
