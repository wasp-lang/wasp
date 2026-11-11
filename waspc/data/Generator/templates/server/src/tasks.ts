{{={= =}=}}
// The script behind `wasp task run` and the `task` npm script. It lives in the
// server project so tasks can use the server logic, like operations and the
// rest.

import { prisma } from 'wasp/server'
import { runTaskFromCli } from './runTaskFromCli.js'

{=# tasks =}
{=& importStatement =}
{=/ tasks =}

await runTaskFromCli(
  {
    {=# tasks =}
    {= taskName =}: withContext({= importIdentifier =}, {
      entities: {
        {=# entities =}
        {= name =}: prisma.{= prismaIdentifier =},
        {=/ entities =}
      },
    }),
    {=/ tasks =}
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
