import { prisma } from 'wasp/server'
import type { JSONValue, JSONObject } from 'wasp/core/serialization'
import { type JobFn, createJobDefinition } from 'wasp/server/jobs/core/pgBoss'

const entities = {
}

// PUBLIC API
export type ScheduledJobWithArgs<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>

const jobSchedule = {
  cron: "0 * * * *",
  args: {"foo":"bar"},
  options: {"retryLimit":2},
}

// PUBLIC API
export const scheduledJobWithArgs = createJobDefinition({
  jobName: 'scheduledJobWithArgs',
  defaultJobOptions: {},
  jobSchedule,
  entities,
})
