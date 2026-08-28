import { initializeServerRuntime } from 'wasp/server/runtime'
import { serverRuntimeBindings } from '../runtimeBindings.js'

async function bootstrapSeed() {
  initializeServerRuntime(serverRuntimeBindings)
  const { runSeed } = await import('./initialization.js')
  await runSeed()
}

bootstrapSeed().catch((error) => {
  console.error(error)
  process.exitCode = 1
})
