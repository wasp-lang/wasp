import type { QueryClient } from "@tanstack/react-query";
import { initializeQueryClient } from "./queryClient";

import { setup as setup_ext } from 'wasp/src/clientSetup'

// PRIVATE API (framework code)
// Composes the user's client setup (which may call `configureQueryClient`)
// with the QueryClient creation. Consumers `await` or `React.use()` this.
export const queryClientPromise: Promise<QueryClient> =
  Promise.resolve(setup_ext()).then(() => initializeQueryClient());
