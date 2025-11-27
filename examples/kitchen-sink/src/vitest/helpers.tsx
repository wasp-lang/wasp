import { QueryClient, QueryClientProvider } from "@tanstack/react-query";
import { cleanup, render, RenderResult } from "@testing-library/react";
import { http, HttpResponse, JsonBodyType } from "msw";
import { setupServer, type SetupServer } from "msw/node";
import { ReactElement, ReactNode } from "react";
import { BrowserRouter as Router } from "react-router-dom";
import { afterAll, afterEach, beforeAll } from "vitest";
import { config, HttpMethod, Route } from "wasp/client";
import { Query } from "wasp/client/operations/rpc";
import { serialize } from "wasp/core/serialization";

export type MockQuery = <Input, Output, MockOutput extends Output>(
  query: Query<Input, Output>,
  resJson: MockOutput,
) => void;

export type MockApi = <Payload extends JsonBodyType>(
  route: Route,
  resJson: Payload,
) => void;

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

// PUBLIC API
export function mockServer(): {
  server: SetupServer;
  mockQuery: MockQuery;
  mockApi: MockApi;
} {
  const server: SetupServer = setupServer();

  beforeAll(() => server.listen());
  afterEach(() => {
    server.resetHandlers();
    cleanup();
  });
  afterAll(() => server.close());

  const mockQuery: MockQuery = (query, mockData) => {
    const route = (query as unknown as { route: Route }).route;
    mockRoute(server, route, () => HttpResponse.json(serialize(mockData)));
  };

  const mockApi: MockApi = (route, mockData) => {
    mockRoute(server, route, () => HttpResponse.json(mockData));
  };

  return { server, mockQuery, mockApi };
}

function mockRoute(
  server: SetupServer,
  route: Route,
  responseHandler: () => Response,
) {
  if (!Object.values(HttpMethod).includes(route.method)) {
    throw new Error(
      `Unsupported query method for mocking: ${
        route.method
      }. Supported method strings are: ${Object.values(HttpMethod).join(", ")}.`,
    );
  }

  const url = `${config.apiUrl}${route.path}`;

  const handlers: Record<HttpMethod, Parameters<typeof server.use>[0]> = {
    [HttpMethod.Get]: http.get(url, responseHandler),
    [HttpMethod.Post]: http.post(url, responseHandler),
    [HttpMethod.Put]: http.put(url, responseHandler),
    [HttpMethod.Delete]: http.delete(url, responseHandler),
  };

  server.use(handlers[route.method]);
}
