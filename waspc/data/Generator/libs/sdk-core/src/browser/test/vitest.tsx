import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { render, type RenderResult } from "@testing-library/react";
import type { ReactElement, ReactNode } from "react";
import { BrowserRouter as Router } from "react-router";
import type { Route } from "../../http.js";
import type { Query } from "../../operations/rpc.js";

// PRIVATE API
export type MockQuery = <Input, Output, MockOutput extends Output>(
  query: Query<Input, Output>,
  resJson: MockOutput,
) => void;

// PRIVATE API
export type MockApi = (route: Route, resJson: unknown) => void;

// PUBLIC API
// Inspired by the Tanstack React Query helper:
// https://github.com/TanStack/query/blob/4ae99561ca3383d6de3f4aad656a49ba4a17b57a/packages/react-query/src/__tests__/utils.tsx#L7-L26
export function renderInContext(ui: ReactElement): RenderResult {
  const client = new QueryClient();
  const { rerender, ...result } = render(
    <QueryClientProvider client={client}>
      <Router>{ui}</Router>
    </QueryClientProvider>,
  );
  return {
    ...result,
    rerender: (rerenderUi: ReactNode) =>
      rerender(
        <QueryClientProvider client={client}>
          <Router>{rerenderUi}</Router>
        </QueryClientProvider>,
      ),
  };
}
