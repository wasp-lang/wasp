import { registerJob } from 'wasp/server/jobs/core/pgBoss'
import { foo } from '../../../../../src/server/jobs/bar.js'
import { scheduledJobWithArgs as _waspJobDefinition } from 'wasp/server/jobs'

registerJob({
  job: _waspJobDefinition,
  jobFn: foo,
})
