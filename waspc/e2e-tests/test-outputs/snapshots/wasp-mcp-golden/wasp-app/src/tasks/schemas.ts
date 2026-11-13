import { z } from "zod";

export const getTasksInput = z.object({
  done: z.boolean().optional().describe("Only tasks with this status."),
});
export type GetTasksInput = z.infer<typeof getTasksInput>;

export const createTaskInput = z.object({
  description: z.string().min(1).max(200).describe("What needs doing."),
});
export type CreateTaskInput = z.infer<typeof createTaskInput>;

export const taskOut = z.object({
  id: z.number(),
  description: z.string(),
  isDone: z.boolean(),
  createdAt: z.iso.datetime(),
});
export type TaskOut = z.infer<typeof taskOut>;

export const tasksOut = z.object({ tasks: z.array(taskOut) });
export type TasksOut = z.infer<typeof tasksOut>;

export const deleteTaskInput = z.object({ id: z.number().describe("The task's ID.") });
export type DeleteTaskInput = z.infer<typeof deleteTaskInput>;