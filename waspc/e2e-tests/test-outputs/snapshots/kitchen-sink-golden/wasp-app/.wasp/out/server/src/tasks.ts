// The script behind `wasp task run` and the `task` npm script. It lives in the
// server project so tasks can use the server logic, like operations and the
// rest.

import { prisma } from 'wasp/server'
import { runTaskFromCli } from './runTaskFromCli.js'

import { deleteDoneTasks as _waspUserFn_deleteDoneTasks } from "../../../../src/features/cli-tasks/deleteDoneTasks"

await runTaskFromCli(
  {
    deleteDoneTasks: withContext(_waspUserFn_deleteDoneTasks, {
      entities: {
        Task: prisma.task,
      },
    }),
  },
  (taskName) => ({ source: "task", taskName }),
)

// Typing `task` as a parameter, instead of calling it directly, lets it take
// fewer parameters than Wasp passes it.
function withContext<Context>(
  task: (args: string[], context: Context) => unknown,
  context: Context,
) {
  return (args: string[]) => task(args, context)
}
