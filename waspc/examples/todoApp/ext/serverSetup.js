import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'

let someResource = undefined

export const getSomeResource = () => someResource

const setup = async () => {
  await new Promise(resolve => setTimeout(resolve, 2000))
  someResource = 'This resource is now set up.'
  console.log('Custom server setup done!')

  console.log('Kicking off Job...')
  // Or: const runningJob = await mySpecialJob.delay(10).performAsync({ something: "here" })
  const runningJob = await mySpecialJob.performAsync({ something: "here" })
  console.log("Job:", runningJob)
  console.log("runningJob.PgBoss.details()", await runningJob.PgBoss.details())
}

export default setup
