# Testing

Wasp provides helpers for testing your client code with [Vitest](https://vitest.dev/) in the `@wasp.sh/test-utils` package.
Server-side testing is not yet supported, but you can track its progress in [this GitHub issue](https://github.com/wasp-lang/wasp/issues/2049).

## Setting up Vitest for client tests

### Installing the packages

Install Vitest, a browser environment, and Wasp's testing helpers:

```bash
npm install -D vitest jsdom @wasp.sh/test-utils
```

- [`vitest`](https://vitest.dev/): the test runner.
- [`jsdom`](https://github.com/jsdom/jsdom): a browser environment for your component tests.
- [`@wasp.sh/test-utils`](https://www.npmjs.com/package/@wasp.sh/test-utils): the [Wasp helpers](#api-reference) for rendering components and mocking your queries and APIs. npm installs the libraries it builds on along with it: [`@testing-library/react`](https://testing-library.com/docs/react-testing-library/intro/), which renders components and finds elements in them, and [`msw`](https://mswjs.io/), which mocks HTTP requests.

### Configuring Vitest

Vitest reads its configuration from your `vite.config.ts`, so your tests use the same Vite setup as your app. Add a `test` section to it:

```ts title="vite.config.ts" auto-js
// highlight-next-line
import { defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  plugins: [wasp()],
  // highlight-start
  test: {
    environment: "jsdom",
    include: ["src/**/*.{test,spec}.?(c|m)[jt]s?(x)"],
  },
  // highlight-end
});
```
With this config, Vitest runs the test files in your `src` folder (e.g., `TaskList.test.tsx` or `helpers.spec.js`) in `jsdom`, a simulated browser.

If you plan to test React components, install [`@testing-library/jest-dom`](https://github.com/testing-library/jest-dom) for DOM matchers like `toBeInTheDocument()`:

```bash
npm install -D @testing-library/jest-dom
```

Then create a setup file that adds the DOM matchers and removes the rendered components after each test:

```ts title="src/test/setup.ts" auto-js
import "@testing-library/jest-dom/vitest";
import { cleanup } from "@testing-library/react";
import { afterEach } from "vitest";

afterEach(() => {
  cleanup();
});
```

And tell Vitest to run it before your tests:

```ts title="vite.config.ts" auto-js
import { defineConfig } from "vitest/config";
import { wasp } from "wasp/client/vite";

export default defineConfig({
  plugins: [wasp()],
  test: {
    environment: "jsdom",
    // highlight-next-line
    setupFiles: ["./src/test/setup.ts"],
    include: ["src/**/*.{test,spec}.?(c|m)[jt]s?(x)"],
  },
});
```

### Running your tests

Your tests import code from the Wasp SDK, which Wasp generates when it compiles your app.
Keep `wasp start` running in one terminal, and run Vitest in another:

```sh
npx vitest run
```

Alternatively, you can run Vitest in watch mode to follow your app's changes:

```sh
npx vitest
```

If you prefer a UI for your test results, install [`@vitest/ui`](https://vitest.dev/guide/ui) and run `npx vitest --ui`.

## Testing your client code

### Testing plain functions

Functions that don't use React or Wasp need no setup. Import them and test them like in any other project:

```ts title="src/tasks/helpers.ts" auto-js
import type { Task } from "wasp/entities";

export function areThereAnyTasks(tasks: Task[]): boolean {
  return tasks.length !== 0;
}
```

```ts title="src/tasks/helpers.test.ts" auto-js
import { expect, test } from "vitest";
import { areThereAnyTasks } from "./helpers";

test("areThereAnyTasks", () => {
  expect(areThereAnyTasks([])).toBe(false);
});
```

### Testing components

Wasp wraps every page of your app in providers: React Query for queries and actions, and React Router for links.
A component that uses queries, actions, or links fails to render without them.

`renderInContext` from `@wasp.sh/test-utils` renders a component inside the same providers:

```tsx title="src/tasks/TasksHeader.tsx" auto-js
import { Link } from "wasp/client/router";

export function TasksHeader() {
  return (
    <div>
      <h1>Your tasks</h1>
      <Link to="/">Back home</Link>
    </div>
  );
}
```

```tsx title="src/tasks/TasksHeader.test.tsx" auto-js
import { screen } from "@testing-library/react";
import { renderInContext } from "@wasp.sh/test-utils";
import { expect, test } from "vitest";
import { TasksHeader } from "./TasksHeader";

test("links back home", () => {
  renderInContext(<TasksHeader />);

  expect(screen.getByRole("link", { name: "Back home" })).toBeInTheDocument();
});
```

### Mocking your queries

A component that calls `useQuery` sends a request to your server. To test the component without running the server, mock the query's response.

Call `mockServer` once at the top of your test file. It starts a mock server for the file and returns `mockQuery`, which takes a query and the data it should return:

```tsx title="src/tasks/TaskList.tsx" auto-js
import { getTasks, useQuery } from "wasp/client/operations";

export function TaskList() {
  const { data: tasks } = useQuery(getTasks);

  return (
    <ul>
      {tasks?.map((task) => (
        <li key={task.id}>
          <input type="checkbox" checked={task.isDone} readOnly />
          {task.description}
        </li>
      ))}
    </ul>
  );
}
```

```tsx title="src/tasks/TaskList.test.tsx" auto-js
import { screen } from "@testing-library/react";
import { mockServer, renderInContext } from "@wasp.sh/test-utils";
import { expect, test } from "vitest";
import { getTasks } from "wasp/client/operations";
import { TaskList } from "./TaskList";

const { mockQuery } = mockServer();

test("shows the tasks", async () => {
  mockQuery(getTasks, [
    { id: 1, description: "Buy milk", isDone: true, userId: 1 },
  ]);

  renderInContext(<TaskList />);

  await screen.findByText("Buy milk");
  expect(screen.getByRole("checkbox")).toBeChecked();
});
```

`mockServer` resets the mocks after each test, so every test mocks the data it needs.

### Mocking your APIs

If your component calls one of your [APIs](../features/apis.md), mock the API with `mockApi`. It takes the API's HTTP method and path, and the JSON it should respond with:

```tsx title="src/tasks/TaskStats.tsx" auto-js
import { useEffect, useState } from "react";
import { api } from "wasp/client/api";

export function TaskStats() {
  const [doneTasks, setDoneTasks] = useState<number>();

  useEffect(() => {
    api
      .get("/stats")
      .json<{ doneTasks: number }>()
      .then((stats) => setDoneTasks(stats.doneTasks));
  }, []);

  return <p>{doneTasks} tasks done</p>;
}
```

```tsx title="src/tasks/TaskStats.test.tsx" auto-js
import { screen } from "@testing-library/react";
import { mockServer, renderInContext } from "@wasp.sh/test-utils";
import { test } from "vitest";
import { HttpMethod } from "wasp/client";
import { TaskStats } from "./TaskStats";

const { mockApi } = mockServer();

test("shows the stats", async () => {
  mockApi({ method: HttpMethod.Get, path: "/stats" }, { doneTasks: 3 });

  renderInContext(<TaskStats />);

  await screen.findByText("3 tasks done");
});
```

## API reference

`@wasp.sh/test-utils` exports these helpers. It needs `vitest`, `msw`, and `@testing-library/react` as peer dependencies, which npm installs along with it.

- `renderInContext(ui, options?)`

  Renders a React element inside the providers Wasp's client code needs. It takes the same options as [Testing Library's `render`](https://testing-library.com/docs/react-testing-library/api#render), except `wrapper`, and returns the same result.

- `mockServer()`

  Starts an [MSW](https://mswjs.io/) server for the current test file and resets its mocks after each test. Call it once, at the top level of the test file. It returns:

  - `mockQuery(query, data)`: makes `query` return `data`.
  - `mockApi(route, data)`: makes the API at `route` (an object with `method` and `path`) respond with `data` as JSON.
  - `server`: the underlying MSW server, for mocking any other request.

- `WaspTestWrapper`

  The component `renderInContext` wraps your element in. Pass it as the `wrapper` option when you call [Testing Library's `render`](https://testing-library.com/docs/react-testing-library/api#wrapper) or `renderHook` yourself.
