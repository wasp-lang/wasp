import type { AuthenticatedQuery } from "wasp/server/module";
import type { Todo } from "./store.js";
import { requireUser } from "./auth.js";
import { moduleConfig } from "./config.js";

const { userForeignKey } = moduleConfig;

type Entities = { Todo: Todo };

export const getTodos: AuthenticatedQuery<Entities, void, Todo[]> = async (
  _args,
  context,
) => {
  const user = requireUser(context);
  return context.entities.Todo.findMany({
    where: userForeignKey ? { [userForeignKey]: user.id } : undefined,
    orderBy: { id: "asc" },
  });
};
