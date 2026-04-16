import type { QueryClient } from "@tanstack/react-query";
import { initializeQueryClient } from "./queryClient";


// PRIVATE API (framework code)
// Composes the user's client setup (which may call `configureQueryClient`)
// with the QueryClient creation. Consumers `await` or `React.use()` this.
export const queryClientPromise: Promise<QueryClient> =
  Promise.resolve(initializeQueryClient());
