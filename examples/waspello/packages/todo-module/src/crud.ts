import type { AuthOperationContext } from "wasp/server/module";
import type { Todo } from "./store.js";
import { requireUser } from "./auth.js";
import { moduleConfig } from "./config.js";

const { todoEntityName, userForeignKey } = moduleConfig;

export async function getAllOverride(
  _args: unknown,
  context: AuthOperationContext
): Promise<Todo[]> {
  const user = requireUser(context);
  return context.entities[todoEntityName].findMany({
    where: {
      isDone: false,
      ...(userForeignKey && { [userForeignKey]: user.id }),
    },
    orderBy: { id: "asc" },
  });
}
