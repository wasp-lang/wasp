import { QueryClient } from "@tanstack/react-query";
import type { CreateQueryClientFn } from "wasp/client/operations";

export const createQueryClient: CreateQueryClientFn = (waspDefaults) =>
  new QueryClient({
    ...waspDefaults,
    defaultOptions: {
      ...waspDefaults.defaultOptions,
      queries: {
        ...waspDefaults.defaultOptions?.queries,
        refetchOnWindowFocus: false,
      },
    },
  });
