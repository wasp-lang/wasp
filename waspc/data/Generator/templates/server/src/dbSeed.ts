{{={= =}=}}
// The script behind `wasp db seed`. It lives in the server project so seeds
// can use the server logic, like operations and the rest.
// TODO: Consider in the future moving it into a a separate project (maybe db/ ?), while still
//   maintaining access to logic from the server/ .

import { prisma, type DbSeedFn } from 'wasp/server'
import { runTaskFromCli } from './runTaskFromCli.js'

{=# dbSeeds =}
{=& importStatement =}
{=/ dbSeeds =}

await runTaskFromCli(
  {
    {=# dbSeeds =}
    {= seedName =}: withPrisma({= importIdentifier =}),
    {=/ dbSeeds =}
  },
  (seedName) => ({ source: "dbSeed", seedName }),
)

// Typing `seed` as a parameter, instead of calling it directly, lets it take
// fewer parameters than Wasp passes it.
function withPrisma(seed: DbSeedFn) {
  return () => seed(prisma)
}
