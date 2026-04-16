{{={= =}=}}
import type { QueryClient } from "@tanstack/react-query";
import { initializeQueryClient } from "./queryClient";

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}
{=/ setupFn.isDefined =}

// PRIVATE API (framework code)
// Composes the user's client setup (which may call `configureQueryClient`)
// with the QueryClient creation. Consumers `await` or `React.use()` this.
export const queryClientPromise: Promise<QueryClient> =
{=# setupFn.isDefined =}
  Promise.resolve({= setupFn.importIdentifier =}()).then(() => initializeQueryClient());
{=/ setupFn.isDefined =}
{=^ setupFn.isDefined =}
  Promise.resolve(initializeQueryClient());
{=/ setupFn.isDefined =}
