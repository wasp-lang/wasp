# Test Utils Library

Vitest helpers for testing the client code of Wasp apps. The generated Wasp SDK re-exports them from `wasp/client/test`.

Entry points:

- `@wasp/lib-test-utils`
- `@wasp/lib-test-utils/config`

See the [Wasp libs conventions](../README.md) for development and packaging.

`vitest`, `jsdom`, `msw`, and `@testing-library/react` are optional peer dependencies, because every Wasp app installs this lib, but only apps with tests install them.

The `jsdom` range copies Vitest's own peer dependency on it (`*`), because the lib only names it as the test environment and doesn't call any of its APIs.
