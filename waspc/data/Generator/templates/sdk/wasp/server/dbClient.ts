{{={= =}=}}
{=# areThereAnyEntitiesDefined =}
import { PrismaClient as InternalPrismaClient } from '@prisma/client'
import type { RegisteredConfig } from 'wasp/types'
{=# isPrismaSetupFnDefined =} 
import { getPrismaClient } from './dbRegistry.js'

export type UserPrismaClient = RegisteredConfig extends { userPrismaClient: infer T }
  ? T
  : InternalPrismaClient

const dbClient: UserPrismaClient = getPrismaClient();
{=/ isPrismaSetupFnDefined =}
{=^ isPrismaSetupFnDefined =} 
const dbClient = InternalPrismaClient();
{=/ isPrismaSetupFnDefined =}
{=/ areThereAnyEntitiesDefined =}
{=^ areThereAnyEntitiesDefined =}
// * Prisma will not generate a PrismaClient if there no
//   entities in the schema. Trying to init the PrismaClient
//   will throw an error.
// * To avoid throwing an error, we return null if there are no
//   entities in the schema.
const dbClient = null;
{=/ areThereAnyEntitiesDefined =}

// PUBLIC API
export type PrismaClient = typeof dbClient;
// PUBLIC API
export default dbClient;

