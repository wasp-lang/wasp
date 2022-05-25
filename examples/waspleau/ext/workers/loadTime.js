import axios from 'axios'
import { upsertMetric } from './utils.js'

export async function workerFunction({ data } = {}) {
  console.log('loadTime.js workerFunction', data)

  const start = Date.now()
  await axios.get(data.url)
  const end = Date.now()

  const metrics = [{ name: data.name, value: `${end - start}ms` }]

  await Promise.all(metrics.map(upsertMetric))

  return metrics
}
