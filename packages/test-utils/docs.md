<!-- This is the user-facing intro of the API reference. For the developer-facing docs, see README.md. -->

# @wasp/test-utils

The `@wasp/test-utils` package has the helpers for [testing](/docs/advanced/testing) the client code of your Wasp app with [Vitest](https://vitest.dev/):

```tsx title="src/tasks/TaskList.test.tsx"
import { mockServer, renderInContext } from "@wasp/test-utils";
```

The helper for your Vitest config comes from `@wasp/test-utils/config`:

```ts title="vite.config.ts"
import { waspTestConfig } from "@wasp/test-utils/config";
```

## Getting the package

Install it together with its peer dependencies:

```bash
npm install -D @wasp/test-utils vitest msw @testing-library/react jsdom
```

- [`vitest`](https://vitest.dev/) runs your tests.
- [`msw`](https://mswjs.io/) mocks the HTTP requests of your queries and APIs.
- [`@testing-library/react`](https://testing-library.com/docs/react-testing-library/intro/) renders your components.
- [`jsdom`](https://github.com/jsdom/jsdom) is the browser environment that `waspTestConfig` sets by default. You can leave it out if you set another `environment`.

The other peer dependencies (`react`, `react-router`, `@tanstack/react-query`, `superjson` and `@wasp/sdk-core`) are already in your project, because they come with `@wasp/wasp`.

The [testing guide](/docs/advanced/testing) has the full setup.
