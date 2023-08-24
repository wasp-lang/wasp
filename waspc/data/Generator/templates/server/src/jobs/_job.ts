{{={= =}=}}
import prisma from '../dbClient.js'
import type { JSONValue } from '../_types/serialization.js'
import type { Job } from './core/job.js'
import { createJob } from './{= executorJobRelFP =}'
{=& jobPerformFnImportStatement =}

const entities = {
  {=# entities =}
  {= name =}: prisma.{= prismaIdentifier =},
  {=/ entities =}
};

export type {= typeName =}<Input extends object, Output extends JSONValue | void> = Job<Input, Output, typeof entities>["jobFn"]

export const {= jobName =} = createJob({
  jobName: "{= jobName =}",
  jobFn: {= jobPerformFnName =},
  defaultJobOptions: {=& jobPerformOptions =},
  jobSchedule: {=& jobSchedule =},
  entities,
})
