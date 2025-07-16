import { Tag, Task } from "wasp/entities";
import { HttpError } from "wasp/server";
import { type GetTasks } from "wasp/server/operations";

export type TaskWithTags = Task & { tags: Tag[] };

export const getTasks: GetTasks<void, TaskWithTags[]> = (_args, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }

  return context.entities.Task.findMany({
    where: { user: { id: context.user.id } },
    orderBy: { createdAt: "desc" },
    include: {
      tags: {
        orderBy: { name: "asc" },
      },
    },
  });
};
