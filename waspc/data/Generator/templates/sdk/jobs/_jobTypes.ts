{{={= =}=}}
import prisma from 'wasp/server/dbClient'
import type { JSONValue, JSONObject } from 'wasp/server/_types/serialization'
import { type JobFn } from '{= jobExecutorTypesImportPath =}'

export const entities = {
  {=# entities =}
  {= name =}: prisma.{= prismaIdentifier =},
  {=/ entities =}
};

export type {= typeName =}<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>
