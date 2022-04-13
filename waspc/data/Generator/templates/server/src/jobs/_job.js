{{={= =}=}}
import { jobFactory } from './{= jobFactoryName =}.js'
{=& jobPerformFnImportStatement =}

export const {= jobName =} = await jobFactory("{= jobName =}", {= jobPerformFnName =})
