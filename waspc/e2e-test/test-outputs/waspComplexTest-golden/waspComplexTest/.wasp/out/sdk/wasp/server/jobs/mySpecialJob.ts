import { prisma } from 'wasp/server'
import type { JSONValue, JSONObject } from 'wasp/core/serialization'
import { type JobFn, createJobDefinition } from 'wasp/server/jobs/core/pgBoss'

const entities = {
}

// PUBLIC API
export type MySpecialJob<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>

const jobSchedule = null

// PUBLIC API
export const mySpecialJob = createJobDefinition({
  jobName: 'mySpecialJob',
  defaultJobOptions: {},
  jobSchedule,
  entities,
})
