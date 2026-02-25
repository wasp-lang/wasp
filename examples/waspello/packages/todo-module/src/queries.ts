import type { AuthOperationContext } from "wasp/server/module";
import type { Todo } from "./store.js";
import { requireUser } from "./auth.js";
import { moduleConfig } from "./config.js";

const { todoEntityName, userForeignKey } = moduleConfig;

export async function getTodos(
  _args: unknown,
  context: AuthOperationContext
): Promise<Todo[]> {
  const user = requireUser(context);
  return context.entities[todoEntityName].findMany({
    where: userForeignKey ? { [userForeignKey]: user.id } : undefined,
    orderBy: { id: "asc" },
  });
}
