# SDK Core Library

App-independent code used by the generated Wasp SDK.

Entry points:

- `@wasp.sh/lib-sdk-core`
- `@wasp.sh/lib-sdk-core/node`
- `@wasp.sh/lib-sdk-core/browser`
- `@wasp.sh/lib-sdk-core/node/vite`
- `@wasp.sh/lib-sdk-core/browser/test`

See the [Wasp libs conventions](../README.md) for development and packaging.

Builds preserve individual modules and use `.mjs` / `.d.mts` extensions to identify ESM explicitly.
Type export checks run during the build.

The browser entry and session module remain side-effectful to preserve cross-tab session synchronization when unused exports are removed. Component CSS is kept only for used components.
