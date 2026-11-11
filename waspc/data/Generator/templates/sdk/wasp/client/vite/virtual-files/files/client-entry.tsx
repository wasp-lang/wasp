{{={= =}=}}
{=# instrumentation.isDefined =}
{=& instrumentation.importStatement =}

// The instrumentation must run before any other app code, so we import the
// rest of the app only after it finishes.
await {= instrumentation.importIdentifier =}()

await import("{= clientAppPath =}")
{=/ instrumentation.isDefined =}
{=^ instrumentation.isDefined =}
import "{= clientAppPath =}"
{=/ instrumentation.isDefined =}
