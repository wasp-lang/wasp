import { registerJob } from 'wasp/server/jobs/core/pgBoss'
import { returnHello as returnHello__userDefined } from '../../../../../src/server/jobs/returnHello.js'
import { returnHelloJob as returnHelloJob__userDefined } from 'wasp/server/jobs'

registerJob({
  job: returnHelloJob__userDefined,
  jobFn: returnHello__userDefined,
})
