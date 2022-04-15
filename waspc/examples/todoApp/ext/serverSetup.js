import { mySpecialJob } from '@wasp/jobs/mySpecialJob.js'

let someResource = undefined

export const getSomeResource = () => someResource

const setup = async () => {
  await new Promise(resolve => setTimeout(resolve, 2000))
  someResource = 'This resource is now set up.'
  console.log('Custom server setup done!')

  console.log('Kicking off Job...')
  // Or: const runningJob = mySpecialJob.delay(1000).performAsync({ something: "here" })
  const runningJob = mySpecialJob.performAsync({ something: "here" })
  console.log('Waiting for Job result...')
  runningJob.result.then(res => { console.log(res) }).finally(() => { console.log("Job done!") })
}

export default setup
