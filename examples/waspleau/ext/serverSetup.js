/**
 * These Jobs are automatically scheduled by Wasp.
 * However, let's kick them off on server setup to ensure we have data right away.
 */

import { github } from '@wasp/jobs/github.js'
import { loadTime } from '@wasp/jobs/loadTime.js'

export default async function () {
  await github.submit()
  await loadTime.submit({
    url: "https://wasp-lang.dev",
    name: "wasp-lang.dev Load Time"
  })
}
