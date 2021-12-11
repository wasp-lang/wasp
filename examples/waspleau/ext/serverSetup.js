/**
 * Setup Function
 * 
 * This function is what we default export from this module.
 * It's primary duty is to setup the workers functions, the queue,
 * and kick off the jobs that will invoke worker functions.
 * */
export default async () => {
  setupWorkerFunctions()
  await setupQueue()
  await populateQueue()
}


/**
 * Dashboard Data
 * 
 * This is our in-memory data store we send down to the client via a query in dashboard.js.
 * This of course means every time the server is restarted we lose this data and must repopulate.
 * */
const dashboardData = {}
export const getDashboardData = () => Object.values(dashboardData).flat()


/** 
 * Workers
 * 
 * Below are two sample workers. The interface they are expected to conform to are:
 * (1) export a named worker object that has the following shape:
 *    { name: '<must be unique for all workers>', fn: async function(opts) {}, schedule: '<cron syntax>' }
 * (2) the async worker function `fn` should return data to save in-memory and display
 *     in the dashboard of the following shape:
 *    [{ name: '', value: '', updatedAt: '' }, ...]
 * 
 * We use the worker name as the lookup key for the function to run when we process jobs.
 * The returned data is an array, so any given worker can have multiple tiles of data in the dashboard.
 * */
import { githubWorker } from './workers/github.js'
import { loadTimeWorker } from './workers/loadTime.js'

const workers = [githubWorker, loadTimeWorker]
const workerFunctions = {}

const setupWorkerFunctions = () => {
  for (const worker of workers) {
    workerFunctions[worker.name] = worker.fn
  }
}


/**
 * Queue
 * 
 * Here we setup the Redis-backed Bull queue named 'waspleau'.
 * Whenever we process a job in the queue's callback function, we look up the worker name and
 * call the associated worker function. We then store the returned data to send to the client.
 * 
 * There is also a MOCK_DATA env var we check for and pass as an opt in case the worker
 * wants to send mock data while doing active development.
 * 
 * Lastly, we populate the queue with a one-off job on startup for each worker, while also
 * scheduling the repeated jobs using cron syntax.
 * */
import Queue from 'bull'

const queue = new Queue('waspleau', process.env.REDIS_URL || 'redis://127.0.0.1:6379',
  { defaultJobOptions: { removeOnComplete: true, removeOnFail: 100 } }
)

const setupQueue = async () => {
  // Clear entire queue when starting up in dev mode or when forced to.
  // This is helpful because if you change a repeating Job's name or cron schedule,
  // the old job will remain!
  //
  // IMPORTANT: Therefore, if you change a repeating Job's name or cron schedule in production,
  // please make sure to obliterate or otherwise clean. If you are using a clustered setup, it is possible
  // this can create lost jobs if obliterate is run on multiple machines simultaneously. In that case, see below.
  //
  // You can alternatively change below to leverage https://github.com/OptimalBits/bull/blob/master/REFERENCE.md#queueremoverepeatable
  // if you only want to remove your old Job and not obliterate.
  if (process.env.NODE_ENV == 'development' || process.env.OBLITERATE_QUEUE == 'true') {
    console.log('Obliterating waspleau queue...')
    await queue.obliterate({ force: true })
    console.log('Done!')
  }

  queue.process('*', async (job) => {
    const workerName = job.data.workerName
    dashboardData[workerName] = await workerFunctions[workerName]({ mockResponse: process.env.MOCK_DATA == "true" || false })
  })
}

const populateQueue = async () => {
  for (const worker of workers) {
    await queue.add({ workerName: worker.name }) // first run
    await queue.add(worker.name, { workerName: worker.name }, { repeat: { cron: worker.schedule } }) // recurring run
  }
}
