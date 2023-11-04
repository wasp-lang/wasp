import { User, Prisma } from '@prisma/client';

export { Task } from '@prisma/client';

export type Context = {
  user: User;
  entities: {
    Task: Prisma.TaskDelegate<{}>;
    User: Prisma.UserDelegate<{}>;
  };
};
