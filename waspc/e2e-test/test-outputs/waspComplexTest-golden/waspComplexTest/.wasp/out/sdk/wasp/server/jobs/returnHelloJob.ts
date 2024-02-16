import { prisma } from 'wasp/server'
import type { JSONValue, JSONObject } from 'wasp/server/_types/serialization'
import { type JobFn, createJobDefinition } from 'wasp/server/jobs/core/pgBoss'

const entities = {
  User: prisma.user,
}

// PUBLIC API
export type ReturnHelloJob<Input extends JSONObject, Output extends JSONValue | void> = JobFn<Input, Output, typeof entities>

// PUBLIC API
export const returnHelloJob = createJobDefinition({
  jobName: 'returnHelloJob',
  defaultJobOptions: {},
  jobSchedule: null,
  entities,
})
