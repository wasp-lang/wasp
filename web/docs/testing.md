---
title: Testing
---
import useBaseUrl from '@docusaurus/useBaseUrl';

import AddExternalAuthEnvVarsReminder from './_addExternalAuthEnvVarsReminder.md'

:::info
Wasp is in beta, so keep in mind there might be some kinks / bugs, and possibly some changes with testing support in the future.
If you encounter any issues, reach out to us on [Discord](https://discord.gg/rzdnErX) and we will make sure to help you out!
:::

# Web App

Wasp enables you to quickly and easily write both unit tests and React component tests for your frontend code. Becase we already use [Vite](https://vitejs.dev/), we were able to add many additional testing capabilities by giving you an integrated [Vitest](https://vitest.dev/) experience out of the box.

<details>
  <summary>Included Libraries</summary>
  <div>

  [`vitest`](https://www.npmjs.com/package/vitest): Unit test framework with native Vite support.

  [`@vitest/ui`](https://www.npmjs.com/package/@vitest/ui): A nice UI for seeing your tests results.

  [`jsdom`](https://www.npmjs.com/package/jsdom): A web browser test environment for Node.js.

  [`@testing-library/react`](https://www.npmjs.com/package/@testing-library/react) / [`@testing-library/jest-dom`](https://www.npmjs.com/package/@testing-library/jest-dom): Testing helpers.

  [`msw`](https://www.npmjs.com/package/msw): A server mocking library.

  </div>
</details>

## Test File Structure

Unit tests should live under your `src/client` directory and have an extension that is compatible with [these glob pattern defaults](https://vitest.dev/config/#include). Within test files, you can import things to test using relative paths.

## Running Tests

Running `wasp test client` will execute Vitest in watch mode, and watch your Wasp source tree for any changes to compile as well.

- If you want to see a live-updating UI, you can pass a `--ui` option, like so: `wasp test client --ui`
- If you'd like to just run the tests once and exit (for example, in CI), you can pass `run`, like so: `wasp test client run`

In fact, anything after `wasp test client` gets passed to Vitest directly, so check out [their CLI docs](https://vitest.dev/guide/cli.html) for more.

:::note
You should not run `wasp test` while running `wasp start`, as both will attempt to compile and write your project to `.wasp/out`.
:::

## Examples
### Unit Tests

```ts title=src/client/Todo.test.tsx
import { test, expect } from 'vitest'

import { areThereAnyTasks } from './Todo'

test('areThereAnyTasks', () => {
  expect(areThereAnyTasks([])).toBe(false)
})
```

### React Component Tests

```ts title=src/client/Todo.test.tsx
import { test, expect } from 'vitest'
import { screen } from '@testing-library/react'

import { mockQuery, renderWrapped } from '@wasp/vitest.helpers'
import getTasks from '@wasp/queries/getTasks'
import Todo from './Todo'

const mockTasks = [{
  id: 1,
  description: 'test todo 1',
  isDone: true,
  userId: 1
}]

test('handles mock data', async () => {
  mockQuery(getTasks, mockTasks);

  renderWrapped(<Todo />)

  await screen.findByText('test todo 1')

  expect(screen.getByRole('checkbox')).toBeChecked()

  screen.debug()
})
```

#### React Testing Helpers

Wasp provides two React testing helpers:
- `mockQuery`: Takes a Wasp Query to mock and the data to return. This is helpful if your Query uses `useQuery`. Behind the scenes, this uses `msw` to create a server request handler that responds with the provided JSON to an HTTP request for the operation's endpoint. Request handlers are cleared after each test.
- `renderWrapped`: Takes a React component, wraps it inside a `QueryClientProvider` and `Router`, and renders it.

# Server

Coming soon!
