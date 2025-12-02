import { prisma } from 'wasp/server'
import type { JSONValue, JSONObject } from 'wasp/core/serialization'
import { type JobFn, createJobDefinition } from 'wasp/server/jobs/core/pgBoss'

const entities = {
  UppercaseTextRequest: prisma.uppercaseTextRequest,
}

// PUBLIC API
export type UppercaseTextJob<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>

const jobSchedule = null

// PUBLIC API
export const uppercaseTextJob = createJobDefinition({
  jobName: 'uppercaseTextJob',
  defaultJobOptions: {},
  jobSchedule,
  entities,
})
