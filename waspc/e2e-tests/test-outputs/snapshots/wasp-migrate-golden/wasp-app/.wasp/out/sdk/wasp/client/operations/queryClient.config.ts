import type { QueryClientConfig } from '@tanstack/react-query'

let queryClientConfig: QueryClientConfig | undefined;
let isFrozen = false;

// PUBLIC API
export function configureQueryClient(config: QueryClientConfig): void {
  if (isFrozen) {
    throw new Error(
      "Attempted to configure the QueryClient after initialization"
    );
  }

  queryClientConfig = config;
}

// PRIVATE API (framework code) — only queryClient.ts should call this
export function freezeAndGetConfig(): QueryClientConfig | undefined {
  isFrozen = true;
  return queryClientConfig;
}
