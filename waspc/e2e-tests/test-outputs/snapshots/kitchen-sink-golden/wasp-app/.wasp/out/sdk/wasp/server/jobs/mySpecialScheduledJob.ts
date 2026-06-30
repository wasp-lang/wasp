import { prisma } from '../index.js'
import type { JSONValue, JSONObject } from '../../core/serialization/index.js'
import { type JobFn, createJobDefinition } from './core/pgBoss/index.js'

const entities = {
}

// PUBLIC API
export type MySpecialScheduledJob<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>

const jobSchedule = {
  cron: "0 * * * *",
  args: {"foo":"bar"},
  options: {"retryLimit":2},
}

// PUBLIC API
export const mySpecialScheduledJob = createJobDefinition({
  jobName: 'mySpecialScheduledJob',
  defaultJobOptions: {},
  jobSchedule,
  entities,
})
