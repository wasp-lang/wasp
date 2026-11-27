# Contributing to @wasp/test-utils

See the [parent README](../README.md) for development and packaging.

## Important Notes

The doc comments in `src/` are the package's API reference at https://wasp.sh/docs/api/@wasp/test-utils. When linking to other Wasp docs from them, write the full `https://wasp.sh/docs/...` URL rather than a relative path.

`@wasp/test-utils/config` is a separate entry point, because `vite.config.ts` imports it, and the testing libraries must stay out of that import.

`jsdom` is an optional peer dependency, because the package only names it as the default test environment and doesn't call any of its APIs. Its range copies Vitest's own peer dependency on it (`*`).

## Testing

```bash
pnpm run test
```
