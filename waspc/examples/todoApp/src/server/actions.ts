import HttpError from '@wasp/core/HttpError.js'
import { getSomeResource } from './serverSetup.js'
import { Task } from '@wasp/entities'
import {
  CreateTask,
  DeleteCompletedTasks,
  ToggleAllTasks,
  UpdateTaskIsDone,
} from '@wasp/actions/types'
import { emailSender } from '@wasp/email/index.js'

export const createTask: CreateTask<Pick<Task, 'description'>> = async (
  task,
  context
) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  const Task = context.entities.Task

  console.log(
    'New task created! Btw, current value of someResource is: ' +
      getSomeResource()
  )

  return Task.create({
    data: {
      description: task.description,
      user: {
        connect: { id: context.user.id },
      },
    },
  })
}

export const updateTaskIsDone: UpdateTaskIsDone<
  Pick<Task, 'id' | 'isDone'>
> = async ({ id, isDone }, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  // Uncomment to test optimistic updates
  // const sleep = (ms) => new Promise(res => setTimeout(res, ms))
  // await sleep(3000);

  await emailSender.send({
    from: {
      title: 'Wasp Todo App',
      email: 'mihovil@ilakovac.com',
    },
    to: 'martin@wasp-lang.dev',
    subject: 'Task status changed',
    text: `User ${context.user.username} marked task ${id} as ${
      isDone ? 'done' : 'not done'
    }.`,
    html: `<div style="font-size: 18px">User ${
      context.user.username
    } marked task ${id} as ${
      isDone
        ? '<span style="color: lime">done</span>'
        : '<span style="color: tomato">not done</span>'
    }.</div>`,
  })

  const Task = context.entities.Task
  return Task.updateMany({
    where: { id, user: { id: context.user.id } },
    data: { isDone },
  })
}

export const deleteCompletedTasks: DeleteCompletedTasks = async (
  _args,
  context
) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  const Task = context.entities.Task
  await Task.deleteMany({
    where: { isDone: true, user: { id: context.user.id } },
  })
}

export const toggleAllTasks: ToggleAllTasks = async (_args, context) => {
  if (!context.user) {
    throw new HttpError(401)
  }

  const whereIsDone = (isDone: boolean) => ({
    isDone,
    user: { id: context.user.id },
  })
  const Task = context.entities.Task
  const notDoneTasksCount = await Task.count({ where: whereIsDone(false) })

  if (notDoneTasksCount > 0) {
    await Task.updateMany({ where: whereIsDone(false), data: { isDone: true } })
  } else {
    await Task.updateMany({ where: whereIsDone(true), data: { isDone: false } })
  }
}
