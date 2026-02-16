import { registerJob } from 'wasp/server/jobs/core/pgBoss'
import { uppercaseText } from '../../../../../src/features/jobs/uppercaseText'
import { uppercaseTextJob as _waspJobDefinition } from 'wasp/server/jobs'

registerJob({
  job: _waspJobDefinition,
  jobFn: uppercaseText,
})
