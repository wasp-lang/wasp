import HttpError from '@wasp/core/HttpError.js';
import type { GetTasks } from '@wasp/queries/types'
import type { Task } from '@wasp/entities';

//Using TypeScript's new 'satisfies' keyword, it will infer the types of the arguments and return value
export const getTasks = ((_args, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }

  return context.entities.Task.findMany({ where: { user: { id: context.user.id } } });
}) satisfies GetTasks;


// This is also a valid way to type the same function.
export const getTasksAlternativelyTyped: GetTasks<void, Task[]> = async (_args, context) => {
  if (!context.user) {
    throw new HttpError(401);
  }

  return context.entities.Task.findMany({ where: { user: { id: context.user.id } } });
}
