import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { useState, type ReactNode } from "react";
import { BrowserRouter } from "react-router";

/**
 * Wraps its children in the same providers as {@link renderInContext}. Pass it
 * as the
 * [`wrapper`](https://testing-library.com/docs/react-testing-library/api#wrapper)
 * option when you call Testing Library's `render` or `renderHook` yourself.
 *
 * Each wrapper gets its own query client, so tests don't share cached data.
 *
 * @example
 * ```tsx
 * import { renderHook } from "@testing-library/react";
 * import { WaspTestWrapper } from "@wasp/test-utils";
 * import { useTaskCount } from "./useTaskCount";
 *
 * const { result } = renderHook(() => useTaskCount(), {
 *   wrapper: WaspTestWrapper,
 * });
 * ```
 *
 * @component
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
