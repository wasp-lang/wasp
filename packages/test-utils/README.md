# Test Utils Library

Vitest helpers for testing the client code of Wasp apps. The generated Wasp SDK re-exports them from `wasp/client/test`.

Entry points:

- `@wasp/test-utils`
- `@wasp/test-utils/config`

See the [parent README](../README.md#packages-the-generated-code-imports) for development and packaging.

`vitest`, `jsdom`, `msw`, and `@testing-library/react` are optional peer dependencies, because every Wasp app installs this package, but only apps with tests install them.

The `jsdom` range copies Vitest's own peer dependency on it (`*`), because the package only names it as the test environment and doesn't call any of its APIs.
