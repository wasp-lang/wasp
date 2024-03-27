import { SimplePrintJob } from "wasp/server/jobs";
import { type Task } from "wasp/entities";

export const simplePrint: SimplePrintJob<
  {
    name: string;
  },
  {
    tasks: Task[];
  }
> = async (args, context) => {
  const tasks = await context.entities.Task.findMany({});
  return {
    tasks,
  };
};
