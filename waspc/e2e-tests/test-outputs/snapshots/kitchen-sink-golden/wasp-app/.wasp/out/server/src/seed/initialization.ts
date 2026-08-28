
// This is a script that is used to seed the database. It is present here in the server project in
// order to have easy access to the server logic, like operations and the rest, since we often want
// to use that logic in our seeding.

import { prisma, DbSeedFn } from 'wasp/server'

import { devSeedSimple } from '../../../../../src/features/db/seeds'
import { prodSeed } from '../../../../../src/features/db/seeds'

const seeds = {
  devSeedSimple,
  prodSeed,
}

export async function runSeed() {
  try {
    const nameOfSeedToRun = process.env.WASP_DB_SEED_NAME
    if (nameOfSeedToRun) {
      console.log(`Running seed: ${nameOfSeedToRun}`)
    } else {
      console.error('Name of the seed to run not specified!')
    }
    await (seeds[nameOfSeedToRun] satisfies DbSeedFn)(prisma)
  } finally {
    await prisma.$disconnect()
  }
}
