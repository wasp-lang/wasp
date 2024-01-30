{{={= =}=}}

// This is a script that is used to seed the database. It is present here in the server project in
// order to have easy access to the server logic, like operations and the rest, since we often want
// to use that logic in our seeding.
// TODO: Consider in the future moving it into a a separate project (maybe db/ ?), while still
//   maintaining access to logic from the server/ .

import { prisma, DbSeedFn } from 'wasp/server'

{=# dbSeeds =}
{=& importStatement =}
{=/ dbSeeds =}

const seeds = {
  {=# dbSeeds =}
  {= importIdentifier =},
  {=/ dbSeeds =}
}

async function main() {
  const nameOfSeedToRun = process.env.{= dbSeedNameEnvVarName =}
  if (nameOfSeedToRun) {
    console.log(`Running seed: ${nameOfSeedToRun}`)
  } else {
    console.error('Name of the seed to run not specified!')
  }
  await (seeds[nameOfSeedToRun] satisfies DbSeedFn)(prisma)
}

main()
  .then(async () => { await prisma.$disconnect() })
  .catch(async (e) => {
    console.error(e)
    await prisma.$disconnect()
    process.exit(1)
  })
