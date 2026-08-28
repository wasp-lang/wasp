import type { Task } from "wasp/entities";
import { HttpError } from "wasp/server";
import type {
  CreateTask,
  GetAdminReport,
  GetMyTasks,
} from "wasp/server/operations";

/**
 * `getMyTasks` and `createTask` are the uniform surface: `context.user` is a
 * row in this app's own `User` table whichever provider vouched for the
 * request, so with two providers configured nothing here changes at all.
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

/**
 * Declared with `auth: ["wasp"]`, so Wasp itself gates it before this code
 * runs: no session is a 401, a session minted by Clerk is a 403. By the time
 * we execute, the user is guaranteed present and wasp-authenticated -- which
 * is why this handler can skip the usual `context.user` check.
 */
export const getAdminReport: GetAdminReport<
  void,
  { taskCount: number; sessionProviderId: string }
> = async (_args, context) => {
  const taskCount = await context.entities.Task.count();
  return {
    taskCount,
    // Always "wasp" here, enforced by the provider restriction.
    sessionProviderId: context.user!.sessionProviderId,
  };
};
