# SDK Core Library

App-independent code used by the generated Wasp SDK.

Entry points:

- `@wasp/sdk-core`
- `@wasp/sdk-core/node`
- `@wasp/sdk-core/browser`
- `@wasp/sdk-core/node/vite`

See the [parent README](../README.md#packages-the-generated-code-imports) for development and packaging.

Builds preserve individual modules and use `.mjs` / `.d.mts` extensions to identify ESM explicitly.
Type export checks run during the build.

The browser entry and session module remain side-effectful to preserve cross-tab session synchronization when unused exports are removed. Component CSS is kept only for used components.
