---
comments: true
last_checked_with_versions:
  Wasp: "0.21"
  Vitest: 4
---

# Vitest

This guide shows you how to set up [Vitest](https://vitest.dev/) in your Wasp project for unit, component, and server-side tests.

## Setting up Vitest

### 1. Install dependencies

Install Vitest and the supporting testing libraries:

```bash
npm install -D vitest @vitest/ui jsdom @testing-library/react @testing-library/jest-dom msw
```

- [`vitest`](https://vitest.dev/): the test runner
- [`@vitest/ui`](https://vitest.dev/guide/ui): optional browser UI for viewing test results
- [`jsdom`](https://github.com/jsdom/jsdom): simulated browser environment for component tests
- [`@testing-library/react`](https://testing-library.com/docs/react-testing-library/intro/): React component rendering and querying utilities
- [`@testing-library/jest-dom`](https://github.com/testing-library/jest-dom): custom matchers like `toBeInTheDocument()`
- [`msw`](https://mswjs.io/): request mocking for tests that involve Wasp queries or API calls

### 2. Configure `vite.config.ts`

Add the `/// <reference types="vitest/config" />` directive and a `test` block to your `vite.config.ts`:

```ts title="vite.config.ts"
// highlight-next-line
/// <reference types="vitest/config" />
import { wasp } from 'wasp/client/vite'
import { defineConfig } from 'vite'

export default defineConfig({
  plugins: [wasp()],
  server: {
    open: true,
  },
  // highlight-start
  test: {
    globals: true,
    environment: 'jsdom',
    setupFiles: ['./src/test/setup.ts'],
    exclude: ['**/node_modules/**', '**/.wasp/**'],
    env: {
      DATABASE_URL: 'postgresql://localhost/test',
    },
  },
  // highlight-end
})
```

The `env` object sets environment variables for the test process. `DATABASE_URL` is required by Wasp's server env validation when importing server-side code in tests.

### 3. Create the setup file

Create `src/test/setup.ts` to load `@testing-library/jest-dom` matchers and clean up the DOM after each test:

```ts title="src/test/setup.ts"
import '@testing-library/jest-dom/vitest'
import { cleanup } from '@testing-library/react'
import { afterEach } from 'vitest'

afterEach(() => {
  cleanup()
})
```

## Writing tests

### Client component tests

Wasp exports `WaspTestWrapper` from `wasp/client/test`. It wraps your component with the `QueryClientProvider` and `BrowserRouter` that Wasp components expect:

```tsx title="src/components/MyComponent.test.tsx"
import { render, screen } from '@testing-library/react'
import { WaspTestWrapper } from 'wasp/client/test'
import { MyComponent } from './MyComponent'

test('renders', () => {
  render(<MyComponent />, { wrapper: WaspTestWrapper })
  expect(screen.getByText('Hello')).toBeInTheDocument()
})
```

#### `renderInContext` and `mockServer` helpers

For more complex tests you'll want `renderInContext` (a shorthand for rendering with `WaspTestWrapper`) and `mockServer` (wraps [MSW](https://mswjs.io/) with Wasp-aware query and API mocking). Copy both into a single helpers file in your project:

```tsx title="src/test/helpers.tsx"
import type { ReactElement, ReactNode } from 'react'
import { render, type RenderResult } from '@testing-library/react'
import { http } from 'msw'
import { setupServer } from 'msw/node'
import { afterAll, afterEach, beforeAll } from 'vitest'
import {
  WaspTestWrapper,
  getOperationRoute,
  getApiUrl,
  serializeForResponse,
  type Route,
} from 'wasp/client/test'

const httpMethodToHandler = {
  GET: http.get,
  POST: http.post,
  PUT: http.put,
  DELETE: http.delete,
} as const

export function renderInContext(ui: ReactElement): RenderResult {
  const { rerender, ...result } = render(ui, { wrapper: WaspTestWrapper })
  return {
    ...result,
    rerender: (rerenderUi: ReactNode) =>
      rerender(<WaspTestWrapper>{rerenderUi}</WaspTestWrapper>),
  }
}

export function mockServer() {
  const server = setupServer()

  beforeAll(() => server.listen())
  afterEach(() => server.resetHandlers())
  afterAll(() => server.close())

  return {
    server,
    mockQuery(query: { route: Route }, mockData: unknown) {
      const { method, url } = getOperationRoute(query)
      server.use(
        httpMethodToHandler[method](url, () =>
          Response.json(serializeForResponse(mockData))
        )
      )
    },
    mockApi(route: Route, mockData: unknown) {
      server.use(
        httpMethodToHandler[route.method](getApiUrl(route), () =>
          Response.json(mockData)
        )
      )
    },
  }
}
```

Call `mockServer()` once at the top of each test file that needs it. It registers the `beforeAll`/`afterEach`/`afterAll` lifecycle hooks automatically:

```tsx title="src/components/Todo.test.tsx"
import { screen } from '@testing-library/react'
import { getTasks } from 'wasp/client/operations'
import { mockServer, renderInContext } from '../test/helpers'
import { Todo } from './Todo'

const { mockQuery } = mockServer()

test('renders tasks', async () => {
  mockQuery(getTasks, [{ id: 1, description: 'Buy milk', isDone: false }])

  renderInContext(<Todo />)

  await screen.findByText('Buy milk')
})
```

### Server-side tests

Wasp's test environment defaults to `jsdom` for client code. To test server-side logic like actions and queries, add `// @vitest-environment node` at the top of the file to override it:

```ts title="src/tasks/actions.test.ts"
// @vitest-environment node
import { describe, it, expect } from 'vitest'
import { createTask } from './actions'

describe('createTask', () => {
  it('throws 401 when user is not authenticated', async () => {
    const context = { user: null, entities: {} as any }
    await expect(
      createTask({ description: 'Test task' }, context as any)
    ).rejects.toMatchObject({ statusCode: 401 })
  })
})
```

## Running tests

Run Vitest in watch mode from your project root:

```bash
npx vitest
```

Pass `--ui` to open the interactive test UI:

```bash
npx vitest --ui
```
