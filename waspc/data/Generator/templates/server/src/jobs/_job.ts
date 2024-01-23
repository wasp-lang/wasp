{{={= =}=}}
import { createJob } from './{= jobExecutorRelativePath =}'
import { entities } from '{= jobTypesImportPath =}'
{=& jobPerformFnImportStatement =}

export const {= jobName =} = createJob({
  jobName: "{= jobName =}",
  jobFn: {= jobPerformFnName =},
  defaultJobOptions: {=& jobPerformOptions =},
  jobSchedule: {=& jobSchedule =},
  entities,
})
