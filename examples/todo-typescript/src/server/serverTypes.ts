import { Task as TaskType, User, Prisma } from '@prisma/client';

export type Task = TaskType;

export type Context = {
  user: User;
  entities: {
    Task: Prisma.TaskDelegate<{}>;
    User: Prisma.UserDelegate<{}>;
  };
};
