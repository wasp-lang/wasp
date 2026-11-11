{{={= =}=}}
{=# instrumentation.isDefined =}
{=& instrumentation.importStatement =}

// The instrumentation must run before any other server code, so we import the
// rest of the server only after it finishes.
await {= instrumentation.importIdentifier =}()

await import('./startServer.js')
{=/ instrumentation.isDefined =}
{=^ instrumentation.isDefined =}
import './startServer.js'
{=/ instrumentation.isDefined =}
