import type { GetTodoItems } from "wasp/server/operations";

type TodoItem = {
  id: number;
  description: string;
  isDone: boolean;
};

type TodoItems = {
  items: TodoItem[];
  totalCount: number;
};

type HostUser = {
  id: number;
};

type HostTaskOwnerWhere = {
  user: { id: HostUser["id"] };
};

type HostTaskDelegate = {
  findMany(args: {
    where: HostTaskOwnerWhere;
    orderBy: { id: "desc" };
    take: number;
    select: { id: true; description: true; isDone: true };
  }): Promise<TodoItem[]>;
  count(args: { where: HostTaskOwnerWhere }): Promise<number>;
};

type HostContext = {
  user?: HostUser;
  entities: {
    Task: HostTaskDelegate;
  };
};

export const getTodoItems: GetTodoItems<void, TodoItems, HostContext> = async (
  _args,
  context,
) => {
  if (!context.user) {
    throw new Error("Log in before reading TODOs from this module.");
  }

  const Task = context.entities.Task;
  const where = { user: { id: context.user.id } };
  const [items, totalCount] = await Promise.all([
    Task.findMany({
      where,
      orderBy: { id: "desc" },
      take: 5,
      select: { id: true, description: true, isDone: true },
    }),
    Task.count({ where }),
  ]);

  return { items, totalCount };
};
