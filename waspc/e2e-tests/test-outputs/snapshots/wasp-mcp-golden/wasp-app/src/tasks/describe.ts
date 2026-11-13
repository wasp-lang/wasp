import { prisma } from "wasp/server";
import type { McpDescribeFn } from "wasp/server/mcp";
import type { DeleteTaskInput } from "./schemas";

/** What the confirm page shows for delete_task. */
export const describeDeleteTask: McpDescribeFn<DeleteTaskInput> = async (
  { id },
  { user },
) => {
  const task = await prisma.task.findFirst({
    where: { id, userId: user.id },
  });
  return {
    title: task
      ? "Delete the task \"" + task.description + "\""
      : "Delete task " + id,
    details: "This can't be undone.",
  };
};