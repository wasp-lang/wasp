import type { Query, Route } from "@wasp.sh/lib-sdk-core";
import { http, type HttpResponseResolver } from "msw";
import { setupServer, type SetupServer } from "msw/node";
import { afterAll, afterEach, beforeAll } from "vitest";

export type MockServer = {
  server: SetupServer;
  mockQuery: MockQuery;
  mockApi: MockApi;
};

export type MockQuery = <Input, Output, MockOutput extends Output>(
  query: Query<Input, Output>,
  mockData: MockOutput,
) => void;

export type MockApi = (route: Route, mockData: unknown) => void;

/**
 * Starts an MSW server for the current test file and returns helpers for
 * mocking Wasp queries and APIs. Mocks are reset after each test.
 *
 * @see https://mswjs.io/docs/api/setup-server
 */
export function createMockServer({
  apiUrl,
  serialize,
}: {
  apiUrl: string;
  serialize: (data: unknown) => unknown;
}): MockServer {
  const server = setupServer();

  beforeAll(() => server.listen());
  afterEach(() => server.resetHandlers());
  afterAll(() => server.close());

  const mockRoute = (route: Route, resolver: HttpResponseResolver): void => {
    const createHandler = httpHandlerByMethod[route.method];
    server.use(createHandler(`${apiUrl}${route.path}`, resolver));
  };

  return {
    server,
    mockQuery: (query, mockData) =>
      mockRoute(query.route, () => Response.json(serialize(mockData))),
    mockApi: (route, mockData) =>
      mockRoute(route, () => Response.json(mockData)),
  };
}

const httpHandlerByMethod = {
  GET: http.get,
  POST: http.post,
  PUT: http.put,
  PATCH: http.patch,
  DELETE: http.delete,
  HEAD: http.head,
} satisfies Record<Route["method"], unknown>;
