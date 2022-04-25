{{={= =}=}}
import { createJob } from './{= jobExecutorFilename =}'
{=& jobPerformFnImportStatement =}

export const {= jobName =} = await createJob("{= jobName =}", {= jobPerformFnName =}, {=& jobPerformOptions =})
