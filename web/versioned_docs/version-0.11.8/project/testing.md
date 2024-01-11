---
title: Testing
---

:::info
Wasp is in beta, so keep in mind there might be some kinks / bugs, and possibly some changes with testing support in the future. If you encounter any issues, reach out to us on [Discord](https://discord.gg/rzdnErX) and we will make sure to help you out!
:::

## Testing Your React App

Wasp enables you to quickly and easily write both unit tests and React component tests for your frontend code. Because Wasp uses [Vite](https://vitejs.dev/), we support testing web apps through [Vitest](https://vitest.dev/).

<details>
  <summary>Included Libraries</summary>
  <div>

[`vitest`](https://www.npmjs.com/package/vitest): Unit test framework with native Vite support.

[`@vitest/ui`](https://www.npmjs.com/package/@vitest/ui): A nice UI for seeing your test results.

[`jsdom`](https://www.npmjs.com/package/jsdom): A web browser test environment for Node.js.

[`@testing-library/react`](https://www.npmjs.com/package/@testing-library/react) / [`@testing-library/jest-dom`](https://www.npmjs.com/package/@testing-library/jest-dom): Testing helpers.

[`msw`](https://www.npmjs.com/package/msw): A server mocking library.

  </div>
</details>

### Writing Tests

For Wasp to pick up your tests, they should be placed within the `src/client` directory and use an extension that matches [these glob patterns](https://vitest.dev/config#include). Some of the file names that Wasp will pick up as tests:

- `yourFile.test.ts`
- `YourComponent.spec.jsx`

Within test files, you can import your other source files as usual. For example, if you have a component `Counter.jsx`, you test it by creating a file in the same directory called `Counter.test.jsx` and import the component with `import Counter from './Counter'`.

### Running Tests

Running `wasp test client` will start Vitest in watch mode and recompile your Wasp project when changes are made.

- If you want to see a realtime UI, pass `--ui` as an option.
- To run the tests just once, use `wasp test client run`.

All arguments after `wasp test client` are passed directly to the Vitest CLI, so check out [their documentation](https://vitest.dev/guide/cli.html) for all of the options.

:::warning Be Careful
You should not run `wasp test` while `wasp start` is running. Both will try to compile your project to `.wasp/out`.
:::

### React Testing Helpers

Wasp provides several functions to help you write React tests:

- `renderInContext`: Takes a React component, wraps it inside a `QueryClientProvider` and `Router`, and renders it. This is the function you should use to render components in your React component tests.

  ```js
  import { renderInContext } from "@wasp/test";

  renderInContext(<MainPage />);
  ```

- `mockServer`: Sets up the mock server and returns an object containing the `mockQuery` and `mockApi` utilities. This should be called outside of any test case, in each file that wants to use those helpers.

  ```js
  import { mockServer } from "@wasp/test";

  const { mockQuery, mockApi } = mockServer();
  ```

  - `mockQuery`: Takes a Wasp [query](/docs/data-model/operations/queries) to mock and the JSON data it should return.

    ```js
    import getTasks from "@wasp/queries/getTasks";

    mockQuery(getTasks, []);
    ```

    - Helpful when your component uses `useQuery`.
    - Behind the scenes, Wasp uses [`msw`](https://npmjs.com/package/msw) to create a server request handle that responds with the specified data.
    - Mock are cleared between each test.

  - `mockApi`: Similar to `mockQuery`, but for [APIs](/docs/advanced/apis). Instead of a Wasp query, it takes a route containing an HTTP method and a path.

    ```js
    import { HttpMethod } from "@wasp/types";

    mockApi({ method: HttpMethod.Get, path: "/foor/bar" }, { res: "hello" });
    ```

## Testing Your Server-Side Code

Wasp currently does not provide a way to test your server-side code, but we will be adding support soon. You can track the progress at [this GitHub issue](https://github.com/wasp-lang/wasp/issues/110) and express your interest by commenting.

## Examples

You can see some tests in a Wasp project [here](https://github.com/wasp-lang/wasp/blob/release/waspc/examples/todoApp/src/client/pages/auth/helpers.test.ts).

### Client Unit Tests

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```js title="src/client/helpers.js"
export function areThereAnyTasks(tasks) {
  return tasks.length === 0;
}
```

```js title="src/client/helpers.test.js"
import { test, expect } from "vitest";

import { areThereAnyTasks } from "./helpers";

test("areThereAnyTasks", () => {
  expect(areThereAnyTasks([])).toBe(false);
});
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```ts title="src/client/helpers.ts"
import { Task } from "@wasp/entities";

export function areThereAnyTasks(tasks: Task[]): boolean {
  return tasks.length === 0;
}
```

```ts title="src/client/helpers.test.ts"
import { test, expect } from "vitest";

import { areThereAnyTasks } from "./helpers";

test("areThereAnyTasks", () => {
  expect(areThereAnyTasks([])).toBe(false);
});
```

</TabItem>
</Tabs>

### React Component Tests

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/Todo.jsx"
import { useQuery } from "@wasp/queries";
import getTasks from "@wasp/queries/getTasks";

const Todo = (_props) => {
  const { data: tasks } = useQuery(getTasks);
  return (
    <ul>
      {tasks &&
        tasks.map((task) => (
          <li key={task.id}>
            <input type="checkbox" value={task.isDone} />
            {task.description}
          </li>
        ))}
    </ul>
  );
};
```

```js title=src/client/Todo.test.jsx
import { test, expect } from "vitest";
import { screen } from "@testing-library/react";

import { mockServer, renderInContext } from "@wasp/test";
import getTasks from "@wasp/queries/getTasks";
import Todo from "./Todo";

const { mockQuery } = mockServer();

const mockTasks = [
  {
    id: 1,
    description: "test todo 1",
    isDone: true,
    userId: 1,
  },
];

test("handles mock data", async () => {
  mockQuery(getTasks, mockTasks);

  renderInContext(<Todo />);

  await screen.findByText("test todo 1");

  expect(screen.getByRole("checkbox")).toBeChecked();

  screen.debug();
});
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/client/Todo.tsx"
import { useQuery } from "@wasp/queries";
import getTasks from "@wasp/queries/getTasks";

const Todo = (_props: {}) => {
  const { data: tasks } = useQuery(getTasks);

  return (
    <ul>
      {tasks &&
        tasks.map((task) => (
          <li key={task.id}>
            <input type="checkbox" value={task.isDone} />
            {task.description}
          </li>
        ))}
    </ul>
  );
};
```

```tsx title=src/client/Todo.test.tsx
import { test, expect } from "vitest";
import { screen } from "@testing-library/react";

import { mockServer, renderInContext } from "@wasp/test";
import getTasks from "@wasp/queries/getTasks";
import Todo from "./Todo";

const { mockQuery } = mockServer();

const mockTasks = [
  {
    id: 1,
    description: "test todo 1",
    isDone: true,
    userId: 1,
  },
];

test("handles mock data", async () => {
  mockQuery(getTasks, mockTasks);

  renderInContext(<Todo />);

  await screen.findByText("test todo 1");

  expect(screen.getByRole("checkbox")).toBeChecked();

  screen.debug();
});
```

</TabItem>
</Tabs>

### Testing With Mocked APIs

<Tabs groupId="js-ts">
<TabItem value="js" label="JavaScript">

```jsx title="src/client/Todo.jsx"
import api from "@wasp/api";

const Todo = (_props) => {
  const [tasks, setTasks] = useState([]);
  useEffect(() => {
    api
      .get("/tasks")
      .then((res) => res.json())
      .then((tasks) => setTasks(tasks))
      .catch((err) => window.alert(err));
  });

  return (
    <ul>
      {tasks &&
        tasks.map((task) => (
          <li key={task.id}>
            <input type="checkbox" value={task.isDone} />
            {task.description}
          </li>
        ))}
    </ul>
  );
};
```

```jsx title=src/client/Todo.test.jsx
import { test, expect } from "vitest";
import { screen } from "@testing-library/react";

import { mockServer, renderInContext } from "@wasp/test";
import Todo from "./Todo";

const { mockApi } = mockServer();

const mockTasks = [
  {
    id: 1,
    description: "test todo 1",
    isDone: true,
    userId: 1,
  },
];

test("handles mock data", async () => {
  mockApi("/tasks", { res: mockTasks });

  renderInContext(<Todo />);

  await screen.findByText("test todo 1");

  expect(screen.getByRole("checkbox")).toBeChecked();

  screen.debug();
});
```

</TabItem>
<TabItem value="ts" label="TypeScript">

```tsx title="src/client/Todo.tsx"
import { Task } from "@wasp/entities";
import api from "@wasp/api";

const Todo = (_props: {}) => {
  const [tasks, setTasks] = useState<Task>([]);
  useEffect(() => {
    api
      .get("/tasks")
      .then((res) => res.json() as Task[])
      .then((tasks) => setTasks(tasks))
      .catch((err) => window.alert(err));
  });

  return (
    <ul>
      {tasks &&
        tasks.map((task) => (
          <li key={task.id}>
            <input type="checkbox" value={task.isDone} />
            {task.description}
          </li>
        ))}
    </ul>
  );
};
```

```tsx title=src/client/Todo.test.tsx
import { test, expect } from "vitest";
import { screen } from "@testing-library/react";

import { mockServer, renderInContext } from "@wasp/test";
import Todo from "./Todo";

const { mockApi } = mockServer();

const mockTasks = [
  {
    id: 1,
    description: "test todo 1",
    isDone: true,
    userId: 1,
  },
];

test("handles mock data", async () => {
  mockApi("/tasks", mockTasks);

  renderInContext(<Todo />);

  await screen.findByText("test todo 1");

  expect(screen.getByRole("checkbox")).toBeChecked();

  screen.debug();
});
```

</TabItem>
</Tabs>
