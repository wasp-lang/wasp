import type { AuthOperationContext } from "wasp/server/module";
import type { Todo } from "./store.js";
import { requireUser } from "./auth.js";
import { moduleConfig } from "./config.js";

const { userForeignKey } = moduleConfig;

type Entities = { Todo: Todo };

export async function getAllOverride(
  _args: unknown,
  context: AuthOperationContext<Entities>
): Promise<Todo[]> {
  const user = requireUser(context);
  return context.entities.Todo.findMany({
    where: {
      isDone: false,
      ...(userForeignKey && { [userForeignKey]: user.id }),
    },
    orderBy: { id: "asc" },
  });
}
