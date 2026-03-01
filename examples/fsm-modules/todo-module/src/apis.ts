import type { Request, Response } from "express";
import type { AuthOperationContext } from "wasp/server/module";
import type { Todo } from "./store.js";
import { requireUser } from "./auth.js";
import { moduleConfig } from "./config.js";

const { userForeignKey } = moduleConfig;

type Entities = { Todo: Todo };

export const getTodosApi = async (
  _req: Request,
  res: Response,
  context: AuthOperationContext<Entities>,
) => {
  const user = requireUser(context);
  const where = userForeignKey ? { [userForeignKey]: user.id } : undefined;
  const result = await context.entities.Todo.findMany({
    where,
    orderBy: { id: "asc" },
  });
  res.json(result);
};

export const getTodoStatsApi = async (
  _req: Request,
  res: Response,
  context: AuthOperationContext<Entities>,
) => {
  const user = requireUser(context);
  const where = userForeignKey ? { [userForeignKey]: user.id } : undefined;
  const total = await context.entities.Todo.count({ where });
  const done = await context.entities.Todo.count({
    where: { isDone: true, ...where },
  });
  res.json({
    total,
    done,
    pending: total - done,
  });
};
