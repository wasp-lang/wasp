import HttpError from 'wasp/core/HttpError'
import type { GetTasks } from 'wasp/server/queries/types'
import type { Task } from 'wasp/entities'
import { ensureValidEmail } from 'wasp/auth/validation'
import { createProviderId } from 'wasp/auth/utils'

//Using TypeScript's new 'satisfies' keyword, it will infer the types of the arguments and return value
export const getTasks = ((_args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  console.log(createProviderId);
  ensureValidEmail({ email: "wasp@gmail.com"});

  return context.entities.Task.findMany({
    where: { user: { id: context.user.id } },
    orderBy: { id: 'asc' },
  })
}) satisfies GetTasks<void, Task[]>
