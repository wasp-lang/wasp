import { startServer } from './start.js'

// This is the production entrypoint of the server: it starts the server and
// exits the process if it can't be started.
startServer().catch((e) => {
  console.error(e)
  process.exit(1)
})
