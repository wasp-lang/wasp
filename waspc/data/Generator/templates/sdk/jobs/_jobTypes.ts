{{={= =}=}}
import prisma from 'wasp/server/dbClient'
import type { JSONValue, JSONObject } from 'wasp/server/_types/serialization'
import { type JobFn } from '{= jobExecutorTypesImportPath =}'

{=! Used in framework code, shouldn't be public =}
export const entities = {
  {=# entities =}
  {= name =}: prisma.{= prismaIdentifier =},
  {=/ entities =}
};

{=! Used by users, should be public =}
export type {= typeName =}<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>
