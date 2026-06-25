import { prisma } from '../index.js'
import type { JSONValue, JSONObject } from '../../core/serialization/index.js'
import { type JobFn, createJobDefinition } from './core/pgBoss/index.js'

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
