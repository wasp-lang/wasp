{{={= =}=}}
import prisma from '../dbClient.js'
import { JSONValue } from '../_types/serialization.js'
import { createJob } from './{= executorJobRelFP =}'
{=& jobPerformFnImportStatement =}

const entities = {
  {=# entities =}
  {= name =}: prisma.{= prismaIdentifier =},
  {=/ entities =}
};

export type {= typeName =}<Input extends JSONValue, Output extends JSONValue> = (args: Input, context: {
  entities: typeof entities
}) => Promise<Output> | void

export const {= jobName =} = createJob({
  jobName: "{= jobName =}",
  jobFn: {= jobPerformFnName =},
  defaultJobOptions: {=& jobPerformOptions =},
  jobSchedule: {=& jobSchedule =},
  entities,
})
