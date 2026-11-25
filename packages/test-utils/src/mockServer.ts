import { stripTrailingSlash, type Query, type Route } from "@wasp/sdk-core";
import { http, type HttpResponseResolver } from "msw";
import { setupServer, type SetupServer } from "msw/node";
import { serialize } from "superjson";
import { afterAll, afterEach, beforeAll } from "vitest";
import { apiUrlEnvVarName } from "./apiUrl.js";

/** The helpers that {@link mockServer} returns. */
export type MockServerResult = {
  /**
   * The underlying [MSW server](https://mswjs.io/docs/api/setup-server), for
   * mocking requests that `mockQuery` and `mockApi` don't cover.
   */
  server: SetupServer;
  mockQuery: MockQuery;
  mockApi: MockApi;
};

/**
 * Mocks the result of a Wasp Query.
 *
 * Wasp registers an [MSW](https://mswjs.io/) request handler that responds to
 * all of the Query's requests with `mockData`, whatever their input.
 *
 * @param query The Query to mock. It can be one from
 * `wasp/client/operations`, or `getMe` from `wasp/client/auth`.
 * @param mockData The data the Query should return. It must match the Query's
 * output type.
 */
export type MockQuery = <Input, Output, MockOutput extends Output>(
  query: Query<Input, Output>,
  mockData: MockOutput,
) => void;

/**
 * Mocks the response of one of your app's
 * [custom APIs](https://wasp.sh/docs/features/apis).
 *
 * @param route The HTTP method and path of the API.
 * @param mockData The data the API should respond with, as JSON.
 */
export type MockApi = (route: Route, mockData: unknown) => void;

/**
 * Sets up a mock server for the current test file, and returns helpers for
 * mocking your Queries and APIs.
 *
 * Call it once, at the top level of each test file that uses the helpers, not
 * inside a test case. It starts an [MSW](https://mswjs.io/) server before the
 * file's tests, resets the mocks after each test, and closes the server after
 * the file's tests.
 *
 * @example
 * ```tsx
 * import { screen } from "@testing-library/react";
 * import { mockServer, renderInContext } from "@wasp/test-utils";
 * import { expect, test } from "vitest";
 * import { HttpMethod } from "wasp/client";
 * import { getTasks } from "wasp/client/operations";
 * import { TaskList } from "./TaskList";
 *
 * const { mockQuery, mockApi } = mockServer();
 *
 * test("shows the tasks", async () => {
 *   mockQuery(getTasks, [
 *     { id: 1, description: "Buy milk", isDone: true, userId: 1 },
 *   ]);
 *   mockApi({ method: HttpMethod.Get, path: "/stats" }, { doneTasks: 3 });
 *
 *   renderInContext(<TaskList />);
 *
 *   await screen.findByText("Buy milk");
 *   expect(screen.getByRole("checkbox")).toBeChecked();
 * });
 * ```
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
      `${apiUrlEnvVarName} is not set. Set it with \`waspTestConfig\` from "@wasp/test-utils/config" in your Vitest config.`,
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
