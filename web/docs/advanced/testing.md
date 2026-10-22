---
title: Testing Wasp client code with Vitest
sidebar_label: Testing
description: Write unit tests and React component tests for the client code of a Wasp app with Vitest, and mock queries and APIs.
---

# Testing

Wasp provides helpers for testing your client code with [Vitest](https://vitest.dev/) in the `@wasp.sh/test-utils` package.

## Setting up Vitest for client tests

### Installing the packages

Install Vitest and the testing libraries:

```bash
npm install -D vitest jsdom@29 @testing-library/jest-dom @wasp.sh/test-utils
```

- [`vitest`](https://vitest.dev/): the test runner.
- [`jsdom`](https://github.com/jsdom/jsdom): a browser environment for your component tests. Version 29 supports all Node.js versions Wasp supports, while version 30 needs Node.js 24.15 or newer.
- [`@testing-library/jest-dom`](https://github.com/testing-library/jest-dom): adds matchers for checking the page, like `toBeInTheDocument()`.
- [`@wasp.sh/test-utils`](https://www.npmjs.com/package/@wasp.sh/test-utils): the [Wasp helpers](#api-reference) for rendering components and mocking your queries and APIs. npm installs the libraries the helpers build on along with it:
  - [`@testing-library/react`](https://testing-library.com/docs/react-testing-library/intro/): renders React components and finds elements in them.
  - [`msw`](https://mswjs.io/): mocks HTTP requests.

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
    setupFiles: ["./src/test/setup.ts"],
    include: ["src/**/*.{test,spec}.?(c|m)[jt]s?(x)"],
  },
  // highlight-end
});
```

With this config, Vitest:

- runs the test files in your `src` folder (e.g., `TaskList.test.tsx` or `helpers.spec.js`),
- runs them in `jsdom`, a simulated browser, so your components can render,
- runs `src/test/setup.ts` before each test file.

Create that setup file:

```ts title="src/test/setup.ts" auto-js
import "@testing-library/jest-dom/vitest";
import { cleanup } from "@testing-library/react";
import { afterEach } from "vitest";

afterEach(() => {
  cleanup();
});
```

It prepares every test file in two ways:

- It adds matchers for checking the page, like `expect(button).toBeInTheDocument()`.
- It removes what a test rendered when the test ends, so the next test starts with an empty page.

### Running your tests

Add a script for running your tests to your `package.json`:

```json title="package.json"
{
  "scripts": {
    "test": "vitest"
  }
}
```

Your tests import code from the Wasp SDK, which Wasp generates when it compiles your app.
Keep `wasp start` running in one terminal, and run your tests in another:

```sh
npm test
```

Vitest watches your files and reruns the affected tests when you change them. In CI, it runs the tests once instead. There, generate the SDK with `wasp compile` before running the tests:

```sh
wasp compile
npm test
```

### Running tests visually

Vitest has a [UI](https://vitest.dev/guide/ui) that shows your test results in the browser. Install it and start Vitest with it:

```sh
npm install -D @vitest/ui
npx vitest --ui
```

## Testing your client code

### Testing plain functions

Functions that don't render components or call your server (through queries, actions, or APIs) need no extra setup. Import them and test them like in any other project:

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

`@wasp.sh/test-utils` exports these helpers.

- `renderInContext(ui, options?)`

  Renders a React element inside the providers Wasp's client code needs. It takes the same options as [Testing Library's `render`](https://testing-library.com/docs/react-testing-library/api#render), except `wrapper`, and returns the same result.

- `mockServer()`

  Starts an [MSW](https://mswjs.io/) server for the current test file and resets its mocks after each test. Call it once, at the top level of the test file. It returns:

  - `mockQuery(query, data)`: makes `query` return `data`.
  - `mockApi(route, data)`: makes the API at `route` (an object with `method` and `path`) respond with `data` as JSON.
  - `server`: the underlying MSW server, for mocking any other request.

- `WaspTestWrapper`

  The component `renderInContext` wraps your element in. Pass it as the `wrapper` option when you call [Testing Library's `render`](https://testing-library.com/docs/react-testing-library/api#wrapper) or `renderHook` yourself.

## Testing server code

Wasp doesn't provide helpers for testing server code yet. You can follow the progress in [this GitHub issue](https://github.com/wasp-lang/wasp/issues/2049).
