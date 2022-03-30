import MySpecialJob from '@wasp/jobs/MySpecialJob.js'

let someResource = undefined

export const getSomeResource = () => someResource

const setup = async () => {
  await new Promise(resolve => setTimeout(resolve, 2000))
  someResource = 'This resource is now set up.'
  console.log('Custom server setup done!')

  console.log('Kicking off job...')
  const job = MySpecialJob.delay(1000).performAsync({ something: "here" })
  console.log('Waiting for job result...')
  job.result().then(res => { console.log(res) }).finally(() => { console.log("Job done") })
}

export default setup
