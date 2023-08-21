{{={= =}=}}
import prisma from '../dbClient.js'
import { createJob } from './{= executorJobRelFP =}'
{=& jobPerformFnImportStatement =}

const entities = {
  {=# entities =}
  {= name =}: prisma.{= prismaIdentifier =},
  {=/ entities =}
};

export type {= typeName =}<Input extends object, Output extends object> = (args: Input, context: {
  entities: typeof entities
}) => Promise<Output> | void

export const {= jobName =} = createJob({
  jobName: "{= jobName =}",
  jobFn: {= jobPerformFnName =},
  defaultJobOptions: {=& jobPerformOptions =},
  jobSchedule: {=& jobSchedule =},
  entities,
})
