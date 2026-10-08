# SDK Core Library

App-independent code used by the generated Wasp SDK.

Entry points: `@wasp.sh/lib-sdk-core`, `@wasp.sh/lib-sdk-core/node`, and
`@wasp.sh/lib-sdk-core/browser`.

See the [Wasp libs conventions](../README.md) for development and packaging.

Builds preserve individual modules and use `.mjs` / `.d.mts` extensions to identify ESM explicitly.
Type export checks run during the build.
