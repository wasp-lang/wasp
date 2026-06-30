import { prisma } from '../index.js'
import type { JSONValue, JSONObject } from '../../core/serialization/index.js'
import { type JobFn, createJobDefinition } from './core/pgBoss/index.js'

const entities = {
  Task: prisma.task,
}

// PUBLIC API
export type MySpecialJob<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>

const jobSchedule = null

// PUBLIC API
export const mySpecialJob = createJobDefinition({
  jobName: 'mySpecialJob',
  defaultJobOptions: {"retryLimit":1},
  jobSchedule,
  entities,
})
