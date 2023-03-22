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

## Included Libraries
- [`vitest`](https://www.npmjs.com/package/vitest): Unit test framework with native Vite support.
  - [`@vitest/ui`](https://www.npmjs.com/package/@vitest/ui): A nice UI for seeing your tests results.
- [`jsdom`](https://www.npmjs.com/package/jsdom): A web browser test environment for Node.js.
- [`@testing-library/react"`](https://www.npmjs.com/package/@testing-library/react) / [`@testing-library/jest-dom`](https://www.npmjs.com/package/@testing-library/jest-dom): Testing helpers.
- [`msw`](https://www.npmjs.com/package/msw): A server mocking library.

## Test File Structure

Unit tests should live under your `src/client` directory and have an extension that is compatible with [these defaults](https://vitest.dev/config/#include). Within test files, you can import things to test using relative paths.

## Unit Testing Example

```ts title=src/client/Todo.test.tsx
import { test, expect } from 'vitest'

import { areThereAnyTasks } from './Todo'

test('areThereAnyTasks', () => {
  expect(areThereAnyTasks([])).toBe(false)
})
```

## React Component Testing Example

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

### React Testing Helpers

Wasp provides two React testing helpers:
- `mockQuery`: Takes a Wasp Query to mock and the data to return. This is helpful if your Query uses `useQuery`. Behind the scenes, this uses `msw` to create a server that responds with JSON to an HTTP request to some operation's endpoint.
- `renderWrapped`: Takes a React component, wraps it inside a `QueryClientProvider` and `Router`, and renders it.

# Server

Coming soon!
