import type { AuthenticatedQueryDefinition } from "wasp/server/module";
import type { Todo } from "./store.js";
import { requireUser } from "./auth.js";
import { moduleConfig } from "./config.js";

const { todoEntityName, userForeignKey } = moduleConfig;

export const getTodos: AuthenticatedQueryDefinition<void, Todo[]> = async (
  _args,
  context,
) => {
  const user = requireUser(context);
  return context.entities[todoEntityName].findMany({
    where: userForeignKey ? { [userForeignKey]: user.id } : undefined,
    orderBy: { id: "asc" },
  });
};
