import type { Query, Route } from "@wasp/lib-sdk-core";
import { http, type HttpResponseResolver } from "msw";
import { setupServer, type SetupServer } from "msw/node";
import { serialize } from "superjson";
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
 * Call it once, at the top level of a test file.
 *
 * @see https://mswjs.io/docs/api/setup-server
 */
export function mockServer(): MockServer {
  const server = setupServer();

  beforeAll(() => server.listen());
  afterEach(() => server.resetHandlers());
  afterAll(() => server.close());

  const mockRoute = (route: Route, resolver: HttpResponseResolver): void => {
    const createHandler = httpHandlerByMethod[route.method];
    // Matching any origin means the helper doesn't need the app's server URL.
    server.use(createHandler(`*${route.path}`, resolver));
  };

  return {
    server,
    mockQuery: (query, mockData) =>
      // The SDK registers its custom types (e.g. Prisma's `Decimal`) on the
      // same `superjson` instance, so they serialize like on the real server.
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
