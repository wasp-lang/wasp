import { HttpError } from 'wasp/server'
import type {
  CreateTask,
  UpdateTask,
  DeleteTasks,
} from 'wasp/server/operations'
import type { Task } from 'wasp/entities'
// import { emailSender } from 'wasp/server/email'
import { printTimeAndNumberOfTasks } from 'wasp/server/jobs'

type CreateArgs = Pick<Task, 'description'>

export const createTask: CreateTask<CreateArgs, Task> = async (
  { description },
  context
) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  console.log("Executing 'printTimeAndNumberOfTasks' task.")
  await printTimeAndNumberOfTasks.submit({})

  // emailSender.send({
  //   to: 'test@example.com',
  //   from: {
  //     name: 'Test',
  //     email: 'test@example.com',
  //   },
  //   subject: 'Test email',
  //   text: 'Thank you for using our app!',
  //   html: '<h1>Thank you for using our app!</h1>',
  // })

  return context.entities.Task.create({
    data: {
      description,
      user: { connect: { id: context.user.id } },
    },
  })
}

type UpdateArgs = Pick<Task, 'id' | 'isDone'>

export const updateTask: UpdateTask<UpdateArgs> = async (
  { id, isDone },
  context
) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  return context.entities.Task.update({
    where: {
      id,
    },
    data: { isDone },
  })
}

export const deleteTasks: DeleteTasks<Task['id'][]> = async (
  idsToDelete,
  context
) => {
  return context.entities.Task.deleteMany({
    where: {
      id: {
        in: idsToDelete,
      },
    },
  })
}
