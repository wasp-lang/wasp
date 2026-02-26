import type { Request, Response } from "express";
import type { AuthOperationContext } from "wasp/server/module";
import { requireUser } from "./auth.js";
import { moduleConfig } from "./config.js";

const { todoEntityName, userForeignKey } = moduleConfig;

export const getTodosApi = async (
  _req: Request,
  res: Response,
  context: AuthOperationContext,
) => {
  const user = requireUser(context);
  const where = userForeignKey ? { [userForeignKey]: user.id } : undefined;
  const result = await context.entities[todoEntityName].findMany({
    where,
    orderBy: { id: "asc" },
  });
  res.json(result);
};

export const getTodoStatsApi = async (
  _req: Request,
  res: Response,
  context: AuthOperationContext,
) => {
  const user = requireUser(context);
  const where = userForeignKey ? { [userForeignKey]: user.id } : undefined;
  const total = await context.entities[todoEntityName].count({ where });
  const done = await context.entities[todoEntityName].count({
    where: { isDone: true, ...where },
  });
  res.json({
    total,
    done,
    pending: total - done,
  });
};
