import type { QueryClient } from "@tanstack/react-query";
import { initializeQueryClient } from "./queryClient";

import { setup as setup_ext } from 'wasp/src/clientSetup'

// PRIVATE API (framework code)
// Composes the user's client setup (which may call `configureQueryClient`)
// with the QueryClient creation. Consumers `await` or `React.use()` this.
//
// NOTE: this module MUST NOT be re-exported through `./index.ts`. User code
// imports `configureQueryClient` from `wasp/client/operations`, and since
// this module imports user code (via `setupFn`), re-exporting it would form
// a cycle whose partially-initialized barrel namespace is snapshotted by
// Vite SSR, causing `configureQueryClient` to appear undefined at call
// time. Consumers should import this module directly instead.
export const queryClientPromise: Promise<QueryClient> =
  Promise.resolve(setup_ext()).then(() => initializeQueryClient());
