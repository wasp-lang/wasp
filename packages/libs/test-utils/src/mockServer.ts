import { stripTrailingSlash, type Query, type Route } from "@wasp/lib-sdk-core";
import { http, type HttpResponseResolver } from "msw";
import { setupServer, type SetupServer } from "msw/node";
import { serialize } from "superjson";
import { afterAll, afterEach, beforeAll } from "vitest";
import { apiUrlEnvVarName } from "./apiUrl.js";

export type MockServerResult = {
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
export function mockServer(): MockServerResult {
  const server = setupServer();

  beforeAll(() => server.listen());
  afterEach(() => server.resetHandlers());
  afterAll(() => server.close());

  const mockRoute = (route: Route, resolver: HttpResponseResolver): void => {
    const createHandler = httpHandlerByMethod[route.method];
    server.use(createHandler(`${getApiUrl()}${route.path}`, resolver));
  };

  return {
    server,
    mockQuery: (query, mockData) =>
      // The generated SDK registers its custom types (e.g. Prisma's `Decimal`)
      // on this same `superjson` instance, so they serialize like on the server.
      mockRoute(query.route, () => Response.json(serialize(mockData))),
    mockApi: (route, mockData) =>
      mockRoute(route, () => Response.json(mockData)),
  };
}

/** The server URL that the app's client sends its requests to in tests. */
function getApiUrl(): string {
  const apiUrl = process.env[apiUrlEnvVarName];
  if (!apiUrl) {
    throw new Error(
      `${apiUrlEnvVarName} is not set. Set it with \`waspTestConfig\` from "wasp/test" in your Vitest config.`,
    );
  }
  return stripTrailingSlash(apiUrl);
}

const httpHandlerByMethod = {
  GET: http.get,
  POST: http.post,
  PUT: http.put,
  PATCH: http.patch,
  DELETE: http.delete,
  HEAD: http.head,
} satisfies Record<Route["method"], unknown>;
