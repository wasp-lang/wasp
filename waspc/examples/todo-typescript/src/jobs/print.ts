import { PrintTimeAndNumberOfTasks } from 'wasp/server/jobs'
import { getTotalNumberOfTasks } from 'wasp/server/operations'

export const printTimeAndNumberOfTasks: PrintTimeAndNumberOfTasks<
  {},
  void
> = async (data, context) => {
  const count = await getTotalNumberOfTasks(null, context)
  console.log(Date.now(), 'Number of tasks:', count)
}
