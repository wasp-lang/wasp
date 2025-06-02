import { type Task } from "wasp/entities";
import { HttpError } from "wasp/server";
import { tasks } from "wasp/server/crud";

export const getTask = (async (args, context) => {
  if (!context.user) {
    throw new HttpError(401, "You must be logged in to view a task.");
  }
  return context.entities.Task.findUnique({
    where: { id: args.id, user: { id: context.user.id } },
    include: {
      user: {},
    },
  });
}) satisfies tasks.GetQuery<{ id: Task["id"] }, {}>;

export const getAllTasks = (async (args, context) => {
  if (!context.user) {
    throw new HttpError(401, "You must be logged in to view tasks.");
  }
  return context.entities.Task.findMany({
    where: {
      user: {
        id: context.user.id,
      },
    },
    orderBy: { id: "desc" },
    select: {
      id: true,
      description: true,
      user: {
        include: {
          auth: {
            include: {
              identities: {
                select: {
                  providerName: true,
                  providerUserId: true,
                },
              },
            },
          },
        },
      },
    },
  });
}) satisfies tasks.GetAllQuery<{}, {}>;

export const createTask = (async (args, context) => {
  if (!context.user) {
    throw new HttpError(401, "You must be logged in to create a task.");
  }
  if (!args.description) {
    throw new HttpError(400, "Task description is required.");
  }
  return context.entities.Task.create({
    data: {
      description: args.description,
      user: {
        connect: {
          id: context.user.id,
        },
      },
    },
  });
}) satisfies tasks.CreateAction<{ description: Task["description"] }, Task>;
