{{={= =}=}}
import { createJob, executorSetup } from './{= jobFilename =}'
{=& jobPerformFnImportStatement =}

await executorSetup({ jobName: "{= jobName =}", jobFn: {= jobPerformFnName =} })

export const {= jobName =} = createJob({ jobName: "{= jobName =}", jobFn: {= jobPerformFnName =}, defaultJobOptions: {=& jobPerformOptions =} })
