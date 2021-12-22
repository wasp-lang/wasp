import axios from 'axios'

const workerFunction = async (opts) => {
  console.log('github.js workerFunction')

  const now = Date.now()

  try {
    const response = opts?.mockResponse
      ? { data: { stargazers_count: 1400, language: 'Haskell', forks: 30, open_issues: 120 } }
      : await axios.get('https://api.github.com/repos/wasp-lang/wasp')

    return [
      { name: 'Wasp GitHub Stars', value: response.data.stargazers_count, updatedAt: now },
      { name: 'Wasp GitHub Language', value: response.data.language, updatedAt: now },
      { name: 'Wasp GitHub Forks', value: response.data.forks, updatedAt: now },
      { name: 'Wasp GitHub Open Issues', value: response.data.open_issues, updatedAt: now },
    ]
  } catch (error) {
    console.error(error)
    return []
  }
}

export const githubWorker = { name: 'GitHub API', fn: workerFunction, schedule: '*/10 * * * *' }
