import { PrintTimeAndNumberOfTasks } from "wasp/jobs/PrintTimeAndNumberOfTasks";
export const printTimeAndNumberOfTasks: PrintTimeAndNumberOfTasks<
  {},
  void
> = async (data, context) => {
  const count = await context.entities.Task.count();
  console.log(Date.now(), "Number of tasks:", count);
};
