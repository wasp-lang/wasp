{{={= =}=}}
{=# areThereAnyEntitiesDefined =}
import Prisma from '@prisma/client'
import { PrismaLibSQL } from '@prisma/adapter-libsql'

function createDbClient(): Prisma.PrismaClient {
  const connectionString = `${process.env.DATABASE_URL}`
  const authToken = `${process.env.DATABASE_AUTH_TOKEN}`

  // Only use libSQL adapter if we're using Turso/libSQL
  if (connectionString.startsWith('libsql://')) {
    const adapter = new PrismaLibSQL({
      url: connectionString,
      authToken,
    })

    return new Prisma.PrismaClient({
      adapter,
    })
  }

  // Default to regular Prisma client for SQLite/PostgreSQL
  return new Prisma.PrismaClient()
}
{=/ areThereAnyEntitiesDefined =}
{=^ areThereAnyEntitiesDefined =}
// * Prisma will not generate a PrismaClient if there no
//   entities in the schema. Trying to init the PrismaClient
//   will throw an error.
// * To avoid throwing an error, we return null if there are no
//   entities in the schema.
function createDbClient(): null {
  return null
}
{=/ areThereAnyEntitiesDefined =}

const dbClient = createDbClient()

// PUBLIC API
export default dbClient
