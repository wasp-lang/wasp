{{={= =}=}}
import { registerJob } from '{= jobExecutorImportPath =}'
{=& jobPerformFn.importStatement =}
{=& jobDefinition.importStatement =}

registerJob({
  job: {= jobDefinition.importIdentifier =},
  jobFn: {= jobPerformFn.importIdentifier =},
})
