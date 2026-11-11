import { serverInstrumentation } from "../../../../src/serverInstrumentation"

// The instrumentation must run before any other server code, so we import the
// rest of the server only after it finishes.
await serverInstrumentation()

await import('./startServer.js')
