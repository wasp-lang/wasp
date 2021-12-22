import axios from 'axios'

const workerFunction = async (opts) => {
  console.log('loadTime.js workerFunction')

  const now = Date.now()

  try {
    if (opts?.mockResponse) {
      return [{ name: 'wasp-lang.dev Load Time', value: "150ms", updatedAt: now }]
    } else {
      const start = Date.now()
      await axios.get('https://api.github.com/repos/wasp-lang/wasp')
      const end = Date.now()
      return [{ name: 'wasp-lang.dev Load Time', value: `${end - start}ms`, updatedAt: now }]
    }
  } catch (error) {
    console.error(error)
    return []
  }
}

export const loadTimeWorker = { name: 'Website Load Time', fn: workerFunction, schedule: '*/5 * * * *' }
