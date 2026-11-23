# Wasp SDK API docs

Docs-only package that TypeDoc uses to generate the API reference of the Wasp SDK (the `wasp` package), published at https://wasp.sh/docs/api/wasp.

The real SDK is generated for each app, so we can't point TypeDoc at it. Instead, `types/` has hand-written `.d.ts` files, one per import path: `types/wasp/server.d.ts` documents `wasp/server`. Keep them in sync with the SDK templates in `waspc/data/Generator/templates/sdk/wasp/`.

> [!NOTE]
> The user-facing intro of the API reference is in [`docs.md`](./docs.md).
