# Test Utils Library

Vitest helpers for testing the client code of Wasp apps. The generated Wasp SDK re-exports them from `wasp/client/test`.

Entry points:

- `@wasp.sh/lib-test-utils`

See the [Wasp libs conventions](../README.md) for development and packaging.

`vitest`, `msw`, and `@testing-library/react` are optional peer dependencies, because every Wasp app installs this lib, but only apps with tests install them.
