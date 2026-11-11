import { prisma } from 'wasp/server'
import { reportUnexpectedServerError, type ServerErrorContext } from 'wasp/server/errors'

type TaskRunner = (args: string[]) => unknown

/**
 * Runs the task named by the first command-line argument, passing it the
 * remaining arguments. App tasks (`wasp task run`) and db seeds
 * (`wasp db seed`) both run through this.
 */
export async function runTaskFromCli(
  tasks: Record<string, TaskRunner>,
  getErrorContext: (taskName: string) => ServerErrorContext,
): Promise<void> {
  const [taskName, ...args] = process.argv.slice(2)

  if (taskName === undefined || !Object.hasOwn(tasks, taskName)) {
    console.error(
      (taskName === undefined ? 'No task name given.' : `There is no task named "${taskName}".`) +
        ` Available tasks: ${Object.keys(tasks).join(', ')}.`
    )
    process.exit(1)
  }

  const task = tasks[taskName]
  try {
    await task(args)
    await prisma.$disconnect()
  } catch (error: unknown) {
    reportUnexpectedServerError(error, getErrorContext(taskName))
    await prisma.$disconnect()
    process.exit(1)
  }
}
