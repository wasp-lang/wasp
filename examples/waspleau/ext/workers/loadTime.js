import axios from 'axios'
import { upsertMetric } from './utils.js'

export async function workerFunction(args) {
  console.log('loadTime.js workerFunction')

  try {
    const start = Date.now()
    await axios.get('https://api.github.com/repos/wasp-lang/wasp')
    const end = Date.now()

    const metrics = [{ name: 'wasp-lang.dev Load Time', value: `${end - start}ms` }]

    await Promise.all(metrics.map(upsertMetric))

    return metrics
  } catch (error) {
    console.error(error)
    return []
  }
}
