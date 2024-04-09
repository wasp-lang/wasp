import { registerJob } from 'wasp/server/jobs/core/pgBoss'
import { foo as foo__userDefined } from '../../../../../src/server/jobs/bar.js'
import { mySpecialJob as mySpecialJob__userDefined } from 'wasp/server/jobs'

registerJob({
  job: mySpecialJob__userDefined,
  jobFn: foo__userDefined,
})
