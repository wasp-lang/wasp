import axios from 'axios'
import { upsertMetric } from './utils.js'

export async function workerFunction(args, context) {
  console.log('loadTime.js workerFunction', args, context)

  const start = Date.now()
  await axios.get(args.url)
  const end = Date.now()

  const metrics = [{ name: args.name, value: `${end - start}ms` }]

  await Promise.all(metrics.map(upsertMetric(context)))

  return metrics
}
