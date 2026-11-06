import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { useState, type ReactNode } from "react";
import { BrowserRouter } from "react-router";

/**
 * Wraps its children in the providers that Wasp's client code needs, so tests
 * can render components that use queries, actions, or links.
 *
 * Each wrapper gets its own query client, so tests don't share cached data.
 * Actions invalidate Wasp's shared query client instead of this one, so Queries
 * rendered in a test don't refetch after an Action.
 *
 * @see https://testing-library.com/docs/react-testing-library/api#wrapper
 */
export function WaspTestWrapper({
  children,
}: {
  children: ReactNode;
}): ReactNode {
  const [queryClient] = useState(() => new QueryClient());

  return (
    <QueryClientProvider client={queryClient}>
      <BrowserRouter>{children}</BrowserRouter>
    </QueryClientProvider>
  );
}
