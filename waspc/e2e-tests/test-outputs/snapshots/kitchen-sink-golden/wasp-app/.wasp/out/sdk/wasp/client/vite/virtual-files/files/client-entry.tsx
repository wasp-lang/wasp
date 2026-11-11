import { clientInstrumentation as clientInstrumentation_ext } from "./src/clientInstrumentation"

// The instrumentation must run before any other app code, so we import the
// rest of the app only after it finishes.
await clientInstrumentation_ext()

await import("/@wasp/client-app.tsx")
