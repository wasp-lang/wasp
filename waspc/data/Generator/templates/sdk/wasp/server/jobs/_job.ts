{{={= =}=}}
import { prisma } from 'wasp/server'
import type { JSONValue, JSONObject } from 'wasp/server/_types/serialization'
import { type JobFn, createJobDefinition } from '{= jobExecutorImportPath =}'

const entities = {
  {=# entities =}
  {= name =}: prisma.{= prismaIdentifier =},
  {=/ entities =}
}

// PUBLIC API
export type {= typeName =}<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>

// PUBLIC API
export const {= jobName =} = createJobDefinition({
  jobName: '{= jobName =}',
  defaultJobOptions: {=& jobPerformOptions =},
  // TODO: jobSchdule template variable is a JSON string
  // and the "args" field is outputted as "null" but it should be "undefined"
  // when the value is not provided
  // @ts-ignore
  jobSchedule: {=& jobSchedule =},
  entities,
})
