import type { Task } from "wasp/entities";
import { HttpError } from "wasp/server";
import type { CreateTask, GetMyTasks } from "wasp/server/operations";

/**
 * These two operations are byte-for-byte identical in all three example apps.
 *
 * That is the entire point of the exercise. `context.user` is a row in this app's
 * own `User` table with this app's own id type, whether the request was verified
 * by Wasp's auth, by Better Auth, or by Clerk.
 */

export const getMyTasks: GetMyTasks<void, Task[]> = async (_args, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }
  return context.entities.Task.findMany({
    where: { userId: context.user.id },
    orderBy: { id: "asc" },
  });
};

export const createTask: CreateTask<{ description: string }, Task> = async (
  { description },
  context,
) => {
  if (!context.user) {
    throw new HttpError(401);
  }
  return context.entities.Task.create({
    data: { description, userId: context.user.id },
  });
};
