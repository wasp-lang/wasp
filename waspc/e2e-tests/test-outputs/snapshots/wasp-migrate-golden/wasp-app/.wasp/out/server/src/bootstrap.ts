import { initializeServerRuntime } from 'wasp/server/runtime'
import { serverRuntimeBindings } from './runtimeBindings.js'

async function bootstrapServer() {
  initializeServerRuntime(serverRuntimeBindings)
  const { startServer } = await import('./initialization.js')
  await startServer()
}

bootstrapServer().catch((error) => {
  console.error(error)
  process.exitCode = 1
})
