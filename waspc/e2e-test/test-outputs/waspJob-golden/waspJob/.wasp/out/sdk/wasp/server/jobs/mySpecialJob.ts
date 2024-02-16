import { prisma } from 'wasp/server'
import type { JSONValue, JSONObject } from 'wasp/server/_types/serialization'
import { type JobFn, createJobDefinition } from 'wasp/server/jobs/core/pgBoss'

const entities = {
}

// PUBLIC API
export type MySpecialJob<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>

// PUBLIC API
export const mySpecialJob = createJobDefinition({
  jobName: 'mySpecialJob',
  defaultJobOptions: {},
  jobSchedule: null,
  entities,
})
