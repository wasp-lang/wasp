import { HttpError } from "wasp/server";

export const createTask = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }
  return context.entities.Task.create({
    data: {
      description: args.description,
      user: { connect: { id: context.user.id } },
    },
  });
};

export const updateTask = async ({ id, isDone }, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }
  return context.entities.Task.update({
    where: { id, user: { id: context.user.id } },
    data: {
      isDone: isDone,
    },
  });
};
