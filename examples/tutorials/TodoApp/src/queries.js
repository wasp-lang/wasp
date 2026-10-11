import { HttpError } from "wasp/errors";

export const getTasks = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }
  return context.entities.Task.findMany({
    where: { user: { id: context.user.id } },
    orderBy: { id: "asc" },
  });
};
