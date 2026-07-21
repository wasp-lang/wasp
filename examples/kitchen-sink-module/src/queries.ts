import type { Query } from "wasp/server/operations";
import type { HostContext, TodoItems } from "./types";

export const getTodoItems: Query<void, TodoItems, HostContext> = async (
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
