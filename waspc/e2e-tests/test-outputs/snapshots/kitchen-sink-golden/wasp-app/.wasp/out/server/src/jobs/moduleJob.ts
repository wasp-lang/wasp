import { registerJob } from 'wasp/server/jobs/core/pgBoss'
import { moduleJob } from '@kitchen-sink/module/moduleJobServer'
import { moduleJob as _waspJobDefinition } from 'wasp/server/jobs'

registerJob({
  job: _waspJobDefinition,
  jobFn: moduleJob,
})
