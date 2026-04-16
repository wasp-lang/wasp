import type { QueryClient } from "@tanstack/react-query";
import { initializeQueryClient } from "./queryClient";

import { setup as setup_ext } from 'wasp/src/clientSetup'

// PRIVATE API (framework code)
// Composes the user's client setup (which may call `configureQueryClient`)
// with the QueryClient creation. Consumers `await` or `React.use()` this.
// We defer the first step to a microtask so that module graph evaluation
// fully completes before the user's `setupFn` runs. This avoids circular
// import issues: user code may `import { configureQueryClient } from
// "wasp/client/operations"`, which re-exports us, and calling into user
// code synchronously during module eval would leave those bindings
// temporarily unresolved.
export const queryClientPromise: Promise<QueryClient> = Promise.resolve()
  .then(() => setup_ext())
  .then(() => initializeQueryClient());
