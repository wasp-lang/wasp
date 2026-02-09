{{={= =}=}}
import { prisma } from 'wasp/server'
import type { JSONValue, JSONObject } from 'wasp/core/serialization'
import { type JobFn, createJobDefinition } from '{= jobExecutorImportPath =}'

const entities = {
  {=# entities =}
  {= name =}: prisma.{= prismaIdentifier =},
  {=/ entities =}
}

// PUBLIC API
export type {= typeName =}<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>

{=# jobSchedule.isDefined =}
const jobSchedule = {
  cron: "{= jobSchedule.cron =}",
  {=# jobSchedule.args.isDefined =}
  args: {=& jobSchedule.args.json =},
  {=/ jobSchedule.args.isDefined =}
  {=# jobSchedule.options.isDefined =}
  options: {=& jobSchedule.options.json =},
  {=/ jobSchedule.options.isDefined =}
  {=^ jobSchedule.options.isDefined =}
  options: {},
  {=/ jobSchedule.options.isDefined =}
}
{=/ jobSchedule.isDefined =}
{=^ jobSchedule.isDefined =}
const jobSchedule = null
{=/ jobSchedule.isDefined =}

// PUBLIC API
export const {= jobName =} = createJobDefinition({
  jobName: '{= jobName =}',
  defaultJobOptions: {=& jobPerformOptions =},
  jobSchedule,
  entities,
})
