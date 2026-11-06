/**
 * Helpers for [testing](https://wasp.sh/docs/advanced/testing) your client code
 * with [Vitest](https://vitest.dev/) and
 * [React Testing Library](https://testing-library.com/docs/react-testing-library/intro/).
 * The helper for configuring Vitest is in {@linkcode "test"! | wasp/test}.
 *
 * @module
 */

import type { Route } from "@wasp/lib-sdk-core";
import type { ReactElement, ReactNode } from "react";
import type { Query } from "./operations";

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
 * _react-query_'s `QueryClientProvider` and React Router's `BrowserRouter`.
 *
 * Use it instead of Testing Library's `render` in your component tests, so your
 * components can use `useQuery`, `useAction`, and links. Each call gets its own
 * query client, so tests don't share cached data. Actions don't invalidate this
 * query client, so Queries rendered in a test don't refetch after an Action.
 *
 * It wraps the element in {@link WaspTestWrapper}.
 *
 * @example
 * ```tsx
 * import { screen } from "@testing-library/react";
 * import { expect, test } from "vitest";
 * import { renderInContext } from "wasp/client/test";
 * import { TasksHeader } from "./TasksHeader";
 *
 * test("links back home", () => {
 *   renderInContext(<TasksHeader />);
 *
 *   expect(screen.getByRole("link", { name: "Back home" })).toBeInTheDocument();
 * });
 * ```
 */
export declare function renderInContext(
  ui: ReactElement,
  options?: RenderOptions,
): RenderResult;

/**
 * Mocks the result of a Wasp Query.
 *
 * Wasp registers an [MSW](https://mswjs.io/) request handler that responds to
 * all of the Query's requests with `mockData`, whatever their input.
 *
 * @param query The Query to mock: one from `wasp/client/operations`, or
 * {@link "client/auth"!getMe | getMe}.
 * @param mockData The data the Query should return. It must match the Query's
 * output type.
 */
type MockQuery = <Input, Output, MockOutput extends Output>(
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
  /**
   * Resets the request handlers to the initial ones, removing those added with
   * `use`, or to `nextHandlers` if you pass any.
   */
  resetHandlers(...nextHandlers: unknown[]): void;
};

/** The helpers that {@link mockServer} returns. */
type MockServer = {
  /**
   * The underlying [MSW](https://mswjs.io/) server, for mocking requests that
   * `mockQuery` and `mockApi` don't cover.
   */
  server: SetupServer;
  mockQuery: MockQuery;
  mockApi: MockApi;
};

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
 * import { expect, test } from "vitest";
 * import { HttpMethod } from "wasp/client";
 * import { getTasks } from "wasp/client/operations";
 * import { mockServer, renderInContext } from "wasp/client/test";
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
export declare function mockServer(): MockServer;

/**
 * Wraps its children in the same providers as {@link renderInContext}. Pass it
 * as the
 * [`wrapper`](https://testing-library.com/docs/react-testing-library/api#wrapper)
 * option when you call Testing Library's `render` or `renderHook` yourself.
 *
 * @example
 * ```tsx
 * import { renderHook } from "@testing-library/react";
 * import { WaspTestWrapper } from "wasp/client/test";
 * import { useTaskCount } from "./useTaskCount";
 *
 * const { result } = renderHook(() => useTaskCount(), {
 *   wrapper: WaspTestWrapper,
 * });
 * ```
 *
 * @component
 */
export declare function WaspTestWrapper(props: {
  children: ReactNode;
}): ReactNode;

// Only the declarations marked with `export` are exported.
export {};
