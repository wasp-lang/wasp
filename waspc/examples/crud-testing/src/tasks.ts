import { type Task } from "wasp/entities";
import { HttpError } from "wasp/server";
import { tasks } from 'wasp/server/crud'

export const getTask = (async (args, context) => {
  return context.entities.Task.findUnique({
    where: { id: args.id },
    include: {
      user: {
        // include: {
        //   auth: {
        //     select: { username: true },
        //   },
        // },
      },
    },
  })
}) satisfies tasks.GetQuery<{ id: Task['id'] }, {}>

export const getAllTasks = (async (args, context) => {
  return context.entities.Task.findMany({
    orderBy: { id: 'desc' },
    select: {
      id: true,
      title: true,
      user: {
        include: {
          auth: {
            include: {
              identities: {
                select: {
                  providerName: true,
                  providerUserId: true,
                }
              },
            },
          },
        },
      },
    },
  })
}) satisfies tasks.GetAllQuery<{}, {}>

export const createTask = (async (args, context) => {
  if (!context.user) {
    throw new HttpError(401, 'You must be logged in to create a task.')
  }
  if (!args.title) {
    throw new HttpError(400, 'Task title is required.')
  }
  return context.entities.Task.create({
    data: {
      title: args.title!,
      user: {
        connect: {
          id: context.user.id,
        },
      },
    },
  })
}) satisfies tasks.CreateAction<{ title: Task['title'] }, Task>
