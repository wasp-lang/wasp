/**
 * Helpers for testing your client code with
 * [Vitest](https://vitest.dev/) and
 * [React Testing Library](https://testing-library.com/docs/react-testing-library/intro/).
 *
 * @module
 */

import type { Query, Route } from "@wasp.sh/lib-sdk-core";
import type { ReactElement } from "react";

/**
 * The options of React Testing Library's
 * [`render`](https://testing-library.com/docs/react-testing-library/api#render-options)
 * function, except `wrapper`.
 *
 * This is a simplified version of Testing Library's `RenderOptions` type; see
 * its docs for all the options.
 */
type RenderOptions = {
  /** The DOM element to render into. Defaults to a new `div` in `baseElement`. */
  container?: HTMLElement;
  /**
   * The element the queries are bound to. Defaults to `container` if you
   * specify it, or to `document.body` otherwise.
   */
  baseElement?: HTMLElement;
  /** Whether to hydrate the `container` instead of rendering into it. */
  hydrate?: boolean;
};

/**
 * The result of React Testing Library's
 * [`render`](https://testing-library.com/docs/react-testing-library/api#render-result)
 * function.
 *
 * This is a simplified version of Testing Library's `RenderResult` type; it
 * also contains all of Testing Library's queries (like `getByText`), bound to
 * `baseElement`.
 */
type RenderResult = {
  /** The DOM element your component was rendered into. */
  container: HTMLElement;
  /** The element the queries are bound to. */
  baseElement: HTMLElement;
  /** Renders a new element into the same container, e.g. to update props. */
  rerender(ui: ReactElement): void;
  /** Unmounts the rendered component. */
  unmount(): void;
  /** Returns a `DocumentFragment` of the rendered component. */
  asFragment(): DocumentFragment;
};

/**
 * Renders a React element inside the providers that Wasp's client code needs:
 * React Query's `QueryClientProvider` and React Router's router.
 *
 * Use it instead of Testing Library's `render` to render components in your
 * React component tests, so they can use Queries, Actions, and links. Each
 * call gets its own query client, so tests don't share cached data.
 *
 * @param ui The element to render.
 * @param options Options for Testing Library's `render`.
 * @returns What Testing Library's `render` returns.
 *
 * @example
 * ```tsx
 * import { test, expect } from "vitest";
 * import { screen } from "@testing-library/react";
 * import { mockServer, renderInContext } from "wasp/client/test";
 * import { getTasks } from "wasp/client/operations";
 * import Todo from "./Todo";
 *
 * const { mockQuery } = mockServer();
 *
 * test("handles mock data", async () => {
 *   mockQuery(getTasks, [
 *     { id: 1, description: "test todo 1", isDone: true, userId: 1 },
 *   ]);
 *
 *   renderInContext(<Todo />);
 *
 *   await screen.findByText("test todo 1");
 *   expect(screen.getByRole("checkbox")).toBeChecked();
 * });
 * ```
 *
 * @see [Testing](https://wasp.sh/docs/advanced/testing)
 */
export declare function renderInContext(
  ui: ReactElement,
  options?: RenderOptions,
): RenderResult;

/**
 * Mocks the result of a Wasp Query.
 *
 * Wasp registers an [MSW](https://mswjs.io/) request handler that responds to
 * the Query's requests with `mockData`. This is helpful when your component
 * uses `useQuery`.
 *
 * @param query The Query to mock, imported from `wasp/client/operations`.
 * @param mockData The data the Query should return. It must match the Query's
 * output type.
 *
 * @example
 * ```ts
 * import { getTasks } from "wasp/client/operations";
 *
 * mockQuery(getTasks, []);
 * ```
 */
type MockQuery = <Input, Output, MockOutput extends Output>(
  query: Query<Input, Output>,
  mockData: MockOutput,
) => void;

/**
 * Mocks the response of one of your app's
 * [custom APIs](https://wasp.sh/docs/features/apis).
 *
 * Similar to {@link MockQuery}, but instead of a Query, it takes the API's
 * HTTP method and path. Wasp responds to the API's requests with `mockData`
 * as JSON.
 *
 * @param route The HTTP method and path of the API.
 * @param mockData The JSON data the API should respond with.
 *
 * @example
 * ```ts
 * import { HttpMethod } from "wasp/client";
 *
 * mockApi({ method: HttpMethod.Get, path: "/foo/bar" }, { res: "hello" });
 * ```
 */
type MockApi = (route: Route, mockData: unknown) => void;

/**
 * An [MSW](https://mswjs.io/) server running in Node.js.
 *
 * This is a simplified version of MSW's `SetupServer` type; see
 * [MSW's docs](https://mswjs.io/docs/api/setup-server) for all its members.
 */
type SetupServer = {
  /** Adds request handlers, which take precedence over the existing ones. */
  use(...handlers: unknown[]): void;
  /** Removes the request handlers added at runtime with `use`. */
  resetHandlers(...nextHandlers: unknown[]): void;
};

/** The helpers that {@link mockServer} returns. */
type MockServer = {
  /**
   * The underlying [MSW](https://mswjs.io/) server, for mocking requests that
   * `mockQuery` and `mockApi` don't cover.
   */
  server: SetupServer;
  /** Mocks the result of a Wasp Query. */
  mockQuery: MockQuery;
  /** Mocks the response of one of your app's custom APIs. */
  mockApi: MockApi;
};

/**
 * Sets up a mock server for the current test file, and returns helpers for
 * mocking your Queries and APIs.
 *
 * Call it once, at the top level of each test file that uses the helpers, not
 * inside a test case. It starts an [MSW](https://mswjs.io/) server before the
 * file's tests, and resets the mocks after each test.
 *
 * Make sure your `devDependencies` include `vitest`.
 *
 * @returns The mock helpers.
 *
 * @example
 * ```tsx
 * import { test, expect } from "vitest";
 * import { screen } from "@testing-library/react";
 * import { HttpMethod } from "wasp/client";
 * import { mockServer, renderInContext } from "wasp/client/test";
 * import { getTasks } from "wasp/client/operations";
 * import Todo from "./Todo";
 *
 * const { mockQuery, mockApi } = mockServer();
 *
 * test("handles mock data", async () => {
 *   mockQuery(getTasks, []);
 *   mockApi({ method: HttpMethod.Get, path: "/foo/bar" }, { res: "hello" });
 *
 *   renderInContext(<Todo />);
 *   // ...
 * });
 * ```
 *
 * @see [Testing](https://wasp.sh/docs/advanced/testing)
 */
export declare function mockServer(): MockServer;

// Only the declarations marked with `export` are exported.
export {};
