import type { Query } from "wasp/server/operations";
import type { HostContext, TodoItem } from "./types";

export const getAllModuleTodos: Query<void, TodoItem[], HostContext> = async (
  _args,
  context,
) => {
  if (!context.user) {
    throw new Error("Log in before reading TODOs from this module.");
  }

  return context.entities.Task.findMany({
    where: { user: { id: context.user.id } },
    orderBy: { id: "desc" },
    select: { id: true, description: true, isDone: true },
  });
};
