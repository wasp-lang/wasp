import { registerJob } from 'wasp/server/jobs/core/pgBoss'
import { mySpecialScheduledJob } from '../../../../../src/features/jobs/bar'
import { mySpecialScheduledJob as _waspJobDefinition } from 'wasp/server/jobs'

registerJob({
  job: _waspJobDefinition,
  jobFn: mySpecialScheduledJob,
})
