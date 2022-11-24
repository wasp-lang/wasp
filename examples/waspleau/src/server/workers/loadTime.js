import axios from 'axios'
import { upsertData } from './utils.js'

export async function workerFunction(args, context) {
  console.log('loadTime.js workerFunction', args, context)

  const start = Date.now()
  await axios.get(args.url)
  const end = Date.now()

  const data = [{ name: args.name, value: `${end - start}ms` }]

  await Promise.all(data.map(upsertData(context)))

  return data
}
