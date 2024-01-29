import { Task } from "wasp/entities";
import { GetAllQuery } from "wasp/server/crud/Tasks";

export const getAllQuery = ((args, context) => {
  return context.entities.Task.findMany({});
}) satisfies GetAllQuery<{}, Task[]>;
