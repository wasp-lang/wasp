import { HttpError, AuthError } from 'wasp/server'
import type { GetTasks } from 'wasp/server/operations'
import type { Task } from 'wasp/entities'
import { ensureValidEmail, createProviderId } from 'wasp/server/auth'

//Using TypeScript's new 'satisfies' keyword, it will infer the types of the arguments and return value
export const getTasks = ((_args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  console.log(AuthError)

  console.log(createProviderId)
  ensureValidEmail({ email: 'wasp@gmail.com' })

  return context.entities.Task.findMany({
    where: { user: { id: context.user.id } },
    orderBy: { id: 'asc' },
  })
}) satisfies GetTasks<void, Task[]>
