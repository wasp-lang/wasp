import axios from 'axios'
import { upsertMetric } from './utils.js'

export async function workerFunction(args, context) {
  console.log('github.js workerFunction', args, context)

  const response = await axios.get('https://api.github.com/repos/wasp-lang/wasp')

  const metrics = [
    { name: 'Wasp GitHub Stars', value: response.data.stargazers_count },
    { name: 'Wasp GitHub Language', value: response.data.language },
    { name: 'Wasp GitHub Forks', value: response.data.forks },
    { name: 'Wasp GitHub Open Issues', value: response.data.open_issues },
  ]

  await Promise.all(metrics.map(upsertMetric(context)))

  return metrics
}
