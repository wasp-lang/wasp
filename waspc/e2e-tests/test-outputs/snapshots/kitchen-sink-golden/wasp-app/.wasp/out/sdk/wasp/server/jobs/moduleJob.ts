import { prisma } from '../index.js'
import type { JSONValue, JSONObject } from '../../core/serialization/index.js'
import { type JobFn, createJobDefinition } from './core/pgBoss/index.js'

const entities = {
}

// PUBLIC API
export type ModuleJob<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>

const jobSchedule = null

// PUBLIC API
export const moduleJob = createJobDefinition({
  jobName: 'moduleJob',
  defaultJobOptions: {},
  jobSchedule,
  entities,
})
